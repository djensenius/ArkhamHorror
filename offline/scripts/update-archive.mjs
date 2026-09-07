#!/usr/bin/env node

import { createHash } from "node:crypto";
import {
  constants,
  createReadStream,
} from "node:fs";
import {
  chmod,
  lstat,
  mkdir,
  open,
  readFile,
  readdir,
} from "node:fs/promises";
import path from "node:path";
import { createGunzip } from "node:zlib";

const textDecoder = new TextDecoder("utf-8", { fatal: true });
const metadataLimit = 1024 * 1024;
const blockSize = 512;

function die(message) {
  throw new Error(message);
}

function decodeUtf8(buffer, label) {
  try {
    return textDecoder.decode(buffer);
  } catch {
    die(`${label} is not valid UTF-8`);
  }
}

function decodeStringField(field, label) {
  const nul = field.indexOf(0);
  const content = nul === -1 ? field : field.subarray(0, nul);
  if (nul !== -1 && field.subarray(nul + 1).some((byte) => byte !== 0)) {
    die(`${label} contains bytes after its NUL terminator`);
  }
  return decodeUtf8(content, label);
}

function parseOctalField(field, label) {
  if ((field[0] & 0x80) !== 0) {
    die(`${label} uses an unsupported base-256 value`);
  }
  const text = field.toString("ascii").replace(/\0.*$/s, "").trim();
  if (!/^[0-7]+$/.test(text)) {
    die(`${label} is not canonical octal`);
  }
  return BigInt(`0o${text}`);
}

function headerChecksum(header) {
  let sum = 0;
  for (let index = 0; index < header.length; index += 1) {
    sum += index >= 148 && index < 156 ? 0x20 : header[index];
  }
  return BigInt(sum);
}

function isZeroBlock(block) {
  return block.every((byte) => byte === 0);
}

function normalizeMemberPath(raw) {
  if (
    raw.length === 0
    || raw.includes("\\")
    || raw.includes("\0")
    || raw.includes("\t")
    || raw.includes("\n")
    || raw.includes("\r")
    || raw.startsWith("/")
  ) {
    die(`unsafe archive member path: ${JSON.stringify(raw)}`);
  }
  let normalized = raw;
  while (normalized.startsWith("./")) {
    normalized = normalized.slice(2);
  }
  normalized = normalized.replace(/\/+$/u, "");
  if (normalized === "") {
    return ".";
  }
  const parts = normalized.split("/");
  if (parts.some((part) => part === "" || part === "." || part === "..")) {
    die(`unsafe archive member path: ${JSON.stringify(raw)}`);
  }
  return parts.join("/");
}

function parsePax(buffer) {
  const values = new Map();
  let offset = 0;
  while (offset < buffer.length) {
    const space = buffer.indexOf(0x20, offset);
    if (space === -1) {
      die("PAX record lacks a length separator");
    }
    const lengthText = buffer.subarray(offset, space).toString("ascii");
    if (!/^[1-9][0-9]*$/.test(lengthText)) {
      die("PAX record has an invalid length");
    }
    const length = Number(lengthText);
    if (!Number.isSafeInteger(length) || length <= space - offset + 2) {
      die("PAX record length is invalid");
    }
    const end = offset + length;
    if (end > buffer.length || buffer[end - 1] !== 0x0a) {
      die("PAX record exceeds its metadata entry");
    }
    const record = buffer.subarray(space + 1, end - 1);
    const equals = record.indexOf(0x3d);
    if (equals <= 0) {
      die("PAX record lacks a key/value separator");
    }
    const keyBuffer = record.subarray(0, equals);
    if (keyBuffer.some((byte) => byte > 0x7f)) {
      die("PAX record key is not ASCII");
    }
    const key = keyBuffer.toString("ascii");
    if (!/^[A-Za-z0-9._-]+$/.test(key)) {
      die("PAX record key is invalid");
    }
    if (["linkpath", "path", "size"].includes(key)) {
      values.set(
        key,
        decodeUtf8(record.subarray(equals + 1), `PAX ${key} value`),
      );
    }
    offset = end;
  }
  return values;
}

class AsyncByteReader {
  constructor(iterable) {
    this.iterator = iterable[Symbol.asyncIterator]();
    this.current = Buffer.alloc(0);
    this.offset = 0;
    this.finished = false;
  }

  async take(maximum) {
    while (this.offset === this.current.length) {
      if (this.finished) {
        return null;
      }
      const next = await this.iterator.next();
      if (next.done) {
        this.finished = true;
        return null;
      }
      this.current = Buffer.from(next.value);
      this.offset = 0;
    }
    const length = Math.min(maximum, this.current.length - this.offset);
    const result = this.current.subarray(this.offset, this.offset + length);
    this.offset += length;
    return result;
  }

  async exactly(length, allowEof = false) {
    const result = Buffer.alloc(length);
    let offset = 0;
    while (offset < length) {
      const chunk = await this.take(length - offset);
      if (chunk === null) {
        if (allowEof && offset === 0) {
          return null;
        }
        die("compressed archive ended mid-entry");
      }
      chunk.copy(result, offset);
      offset += chunk.length;
    }
    return result;
  }
}

function headerPath(header) {
  const name = decodeStringField(header.subarray(0, 100), "tar member name");
  const prefix = decodeStringField(
    header.subarray(345, 500),
    "tar member prefix",
  );
  return prefix ? `${prefix}/${name}` : name;
}

function metadataString(buffer, label) {
  const nul = buffer.indexOf(0);
  const content = nul === -1 ? buffer : buffer.subarray(0, nul);
  if (nul !== -1 && buffer.subarray(nul + 1).some((byte) => byte !== 0)) {
    die(`${label} contains bytes after its NUL terminator`);
  }
  return decodeUtf8(content, label).replace(/\n$/u, "");
}

async function parseTar(archive, limits, visitor) {
  const gunzip = createGunzip();
  const source = createReadStream(archive);
  source.on("error", (error) => gunzip.destroy(error));
  source.pipe(gunzip);
  const reader = new AsyncByteReader(gunzip);
  let localPax = null;
  let globalPax = new Map();
  let longName = null;
  let zeroBlocks = 0;
  let memberCount = 0;
  let totalBytes = 0n;

  while (true) {
    const header = await reader.exactly(blockSize, true);
    if (header === null) {
      die("archive lacks the required two-block end marker");
    }
    if (isZeroBlock(header)) {
      zeroBlocks += 1;
      if (zeroBlocks < 2) {
        continue;
      }
      while (true) {
        const trailing = await reader.take(1024 * 1024);
        if (trailing === null) {
          return;
        }
        if (!isZeroBlock(trailing)) {
          die("archive contains data after its end marker");
        }
      }
    }
    if (zeroBlocks !== 0) {
      die("archive contains a non-zero block after its end marker began");
    }

    memberCount += 1;
    if (memberCount > limits.maxMembers) {
      die("archive member-count limit exceeded");
    }
    const expectedChecksum = parseOctalField(
      header.subarray(148, 156),
      "tar header checksum",
    );
    if (headerChecksum(header) !== expectedChecksum) {
      die("tar header checksum mismatch");
    }

    const rawType = header[156];
    const type = rawType === 0 ? "0" : String.fromCharCode(rawType);
    const sizeBig = parseOctalField(header.subarray(124, 136), "tar member size");
    totalBytes += sizeBig;
    if (totalBytes > limits.maxBytes) {
      die("archive uncompressed-size limit exceeded");
    }
    if (sizeBig > BigInt(Number.MAX_SAFE_INTEGER)) {
      die("archive member is too large");
    }
    const size = Number(sizeBig);
    const modeBig = parseOctalField(header.subarray(100, 108), "tar member mode");
    if (modeBig > 0o7777n) {
      die("tar member mode is invalid");
    }
    const mode = Number(modeBig & 0o777n);
    const bodyChunks = [];
    const collectMetadata = ["x", "g", "L"].includes(type);
    if (collectMetadata && size > metadataLimit) {
      die("archive metadata entry is too large");
    }

    let rawPath = headerPath(header);
    let pax = new Map(globalPax);
    if (localPax !== null) {
      for (const [key, value] of localPax) {
        pax.set(key, value);
      }
    }
    if (type !== "x" && type !== "g" && type !== "L") {
      if (longName !== null) {
        rawPath = longName;
      } else if (pax.has("path")) {
        rawPath = pax.get("path");
      }
      if (pax.has("linkpath")) {
        die("archive member uses a PAX link target");
      }
      if (pax.has("size")) {
        const paxSize = pax.get("size");
        if (!/^(0|[1-9][0-9]*)$/.test(paxSize) || BigInt(paxSize) !== sizeBig) {
          die("PAX size does not match the tar header");
        }
      }
      localPax = null;
      longName = null;
    }

    let entry = null;
    let context = null;
    if (type === "0") {
      entry = {
        kind: "file",
        mode,
        name: normalizeMemberPath(rawPath),
        size,
      };
      context = await visitor.start(entry);
    } else if (type === "5") {
      if (size !== 0) {
        die("archive directory has a non-zero body");
      }
      entry = {
        kind: "dir",
        mode,
        name: normalizeMemberPath(rawPath),
        size: 0,
      };
      context = await visitor.start(entry);
    } else if (!collectMetadata) {
      die(`unsupported archive member type: ${JSON.stringify(rawPath)}`);
    }

    let remaining = size;
    while (remaining > 0) {
      const chunk = await reader.take(Math.min(remaining, 1024 * 1024));
      if (chunk === null) {
        die("archive ended inside a member body");
      }
      if (collectMetadata) {
        bodyChunks.push(Buffer.from(chunk));
      } else {
        await visitor.data(entry, context, chunk);
      }
      remaining -= chunk.length;
    }
    const padding = (blockSize - (size % blockSize)) % blockSize;
    if (padding !== 0) {
      const paddingBytes = await reader.exactly(padding);
      if (!isZeroBlock(paddingBytes)) {
        die("archive member padding is not zero-filled");
      }
    }

    if (collectMetadata) {
      const metadata = Buffer.concat(bodyChunks);
      if (type === "x") {
        localPax = parsePax(metadata);
      } else if (type === "g") {
        const parsed = parsePax(metadata);
        if (parsed.has("path") || parsed.has("linkpath") || parsed.has("size")) {
          die("global PAX metadata cannot redefine member identity");
        }
        globalPax = new Map([...globalPax, ...parsed]);
      } else {
        longName = metadataString(metadata, "GNU long member name");
      }
    } else {
      await visitor.end(entry, context);
    }
  }
}

function releaseLaunchers(platform) {
  if (platform.startsWith("linux-")) {
    return [
      "Start-ArkhamHorror.bat",
      "Update-ArkhamHorror.bat",
      "Update-ArkhamHorror.sh",
    ];
  }
  if (platform.startsWith("macos-")) {
    return [
      "Start-ArkhamHorror.command",
      "Update-ArkhamHorror.command",
      "Update-ArkhamHorror.sh",
    ];
  }
  die(`unsupported package platform: ${platform}`);
}

function allowedEntry(name, kind, launchers) {
  if (name === ".") {
    return kind === "dir";
  }
  if (name === "game" || name.startsWith("game/")) {
    return true;
  }
  if (["backup", "cards", "cards_en"].includes(name)) {
    return kind === "dir";
  }
  return launchers.includes(name);
}

function insertEntry(root, entry) {
  if (entry.name === ".") {
    if (entry.kind !== "dir") {
      die("archive root is not a directory");
    }
    return;
  }
  const parts = entry.name.split("/");
  let node = root;
  for (let index = 0; index < parts.length; index += 1) {
    if (node.kind === "file") {
      die(`file/directory archive collision: ${parts.slice(0, index).join("/")}`);
    }
    const part = parts[index];
    if (!node.children.has(part)) {
      node.children.set(part, { children: new Map(), kind: null });
    }
    node = node.children.get(part);
  }
  if (node.kind !== null) {
    die(`duplicate archive member: ${entry.name}`);
  }
  if (entry.kind === "file" && node.children.size !== 0) {
    die(`file/directory archive collision: ${entry.name}`);
  }
  node.kind = entry.kind;
}

async function scanArchive(archive, limits, platform) {
  const launchers = releaseLaunchers(platform);
  const entries = [];
  const byName = new Map();
  const tree = { children: new Map(), kind: "dir" };
  await parseTar(archive, limits, {
    async start(entry) {
      if (!allowedEntry(entry.name, entry.kind, launchers)) {
        die(`archive contains an unexpected package path: ${entry.name}`);
      }
      insertEntry(tree, entry);
      entries.push(entry);
      byName.set(entry.name, entry);
      return null;
    },
    async data() {},
    async end() {},
  });

  const requireEntry = (name, kind, executable = false) => {
    const entry = byName.get(name);
    if (!entry || entry.kind !== kind) {
      die(`archive lacks required ${kind}: ${name}`);
    }
    if (executable && (entry.mode & 0o111) === 0) {
      die(`archive executable lacks execute permission: ${name}`);
    }
  };
  requireEntry("game", "dir");
  for (const directory of ["backup", "cards", "cards_en"]) {
    requireEntry(directory, "dir");
  }
  for (const file of [
    "game/start.sh",
    "game/update.sh",
    "game/tools/node",
  ]) {
    requireEntry(file, "file", true);
  }
  for (const file of [
    "game/config/release-platform",
    "game/config/release-version",
    "game/tools/update-archive.mjs",
  ]) {
    requireEntry(file, "file");
  }
  for (const launcher of launchers) {
    requireEntry(
      launcher,
      "file",
      launcher.endsWith(".sh") || launcher.endsWith(".command"),
    );
  }
  return { byName, entries, launchers };
}

function selectedForExtraction(name, launchers) {
  return name === "game"
    || name.startsWith("game/")
    || launchers.includes(name);
}

async function extractArchive(archive, destination, limits, expected) {
  const directoryModes = [];
  let index = 0;
  await parseTar(archive, limits, {
    async start(entry) {
      const expectedEntry = expected.entries[index];
      index += 1;
      if (
        !expectedEntry
        || expectedEntry.name !== entry.name
        || expectedEntry.kind !== entry.kind
        || expectedEntry.mode !== entry.mode
        || expectedEntry.size !== entry.size
      ) {
        die("archive changed between preflight and extraction");
      }
      if (!selectedForExtraction(entry.name, expected.launchers)) {
        return null;
      }
      const target = path.join(destination, ...entry.name.split("/"));
      if (entry.kind === "dir") {
        await mkdir(target, { mode: 0o700, recursive: true });
        directoryModes.push([target, entry.mode]);
        return null;
      }
      await mkdir(path.dirname(target), { mode: 0o700, recursive: true });
      return open(target, constants.O_WRONLY | constants.O_CREAT | constants.O_EXCL, 0o600);
    },
    async data(entry, handle, chunk) {
      if (handle === null) {
        return;
      }
      let offset = 0;
      while (offset < chunk.length) {
        const { bytesWritten } = await handle.write(
          chunk,
          offset,
          chunk.length - offset,
        );
        if (bytesWritten <= 0) {
          die(`could not extract archive member: ${entry.name}`);
        }
        offset += bytesWritten;
      }
    },
    async end(entry, handle) {
      if (handle === null) {
        return;
      }
      await handle.close();
      const target = path.join(destination, ...entry.name.split("/"));
      await chmod(target, entry.mode);
    },
  });
  if (index !== expected.entries.length) {
    die("archive changed between preflight and extraction");
  }
  directoryModes.sort((left, right) => right[0].length - left[0].length);
  for (const [directory, mode] of directoryModes) {
    await chmod(directory, mode);
  }
}

function compareVersions(left, right) {
  if (left.date !== right.date) {
    return left.date < right.date ? -1 : 1;
  }
  if (left.sequence === right.sequence) {
    return 0;
  }
  return left.sequence < right.sequence ? -1 : 1;
}

async function selectArchive(base, platform) {
  const escaped = platform.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
  const pattern = new RegExp(
    `^ArkhamHorror-${escaped}-(v([0-9]{8})\\.(0|[1-9][0-9]*))\\.tar\\.gz$`,
    "u",
  );
  const broadPattern = new RegExp(
    `^ArkhamHorror-${escaped}-v[0-9]{8}\\.[0-9]+\\.tar\\.gz$`,
    "u",
  );
  const candidates = [];
  const versions = new Set();
  for (const name of await readdir(base)) {
    const match = pattern.exec(name);
    if (match === null) {
      if (broadPattern.test(name)) {
        die(`same-platform release archive name is not canonical: ${name}`);
      }
      continue;
    }
    const archive = path.join(base, name);
    const metadata = await lstat(archive);
    if (!metadata.isFile() || metadata.isSymbolicLink()) {
      continue;
    }
    const canonical = `${match[2]}.${BigInt(match[3]).toString()}`;
    if (versions.has(canonical)) {
      die(`ambiguous same-platform release version: ${canonical}`);
    }
    versions.add(canonical);
    candidates.push({
      archive,
      date: match[2],
      metadata,
      name,
      sequence: BigInt(match[3]),
      version: match[1],
    });
  }
  if (candidates.length === 0) {
    die(`no same-platform release archive for ${platform}`);
  }
  candidates.sort(compareVersions);
  return candidates.at(-1);
}

async function copyAndHash(candidate, destination, expectedDigest, maximumBytes) {
  const flags = constants.O_RDONLY | (constants.O_NOFOLLOW ?? 0);
  const source = await open(candidate.archive, flags);
  const sourceMetadata = await source.stat();
  if (
    !sourceMetadata.isFile()
    || sourceMetadata.dev !== candidate.metadata.dev
    || sourceMetadata.ino !== candidate.metadata.ino
    || sourceMetadata.size !== candidate.metadata.size
  ) {
    await source.close();
    die("selected release archive changed before verification");
  }
  if (sourceMetadata.size > Number(maximumBytes)) {
    await source.close();
    die("release archive exceeds the size limit");
  }

  const verifiedArchive = path.join(destination, ".verified-release.tar.gz");
  const output = await open(
    verifiedArchive,
    constants.O_WRONLY | constants.O_CREAT | constants.O_EXCL,
    0o400,
  );
  const digest = createHash("sha256");
  const buffer = Buffer.alloc(1024 * 1024);
  let position = 0;
  try {
    while (true) {
      const { bytesRead } = await source.read(
        buffer,
        0,
        buffer.length,
        position,
      );
      if (bytesRead === 0) {
        break;
      }
      const chunk = buffer.subarray(0, bytesRead);
      digest.update(chunk);
      let written = 0;
      while (written < chunk.length) {
        const result = await output.write(
          chunk,
          written,
          chunk.length - written,
          position + written,
        );
        if (result.bytesWritten <= 0) {
          die("could not copy the selected release archive");
        }
        written += result.bytesWritten;
      }
      position += bytesRead;
    }
    await output.sync();
  } finally {
    await source.close();
    await output.close();
  }
  if (digest.digest("hex") !== expectedDigest) {
    die(`published checksum mismatch for ${candidate.name}`);
  }
  return verifiedArchive;
}

async function hashFile(file) {
  const digest = createHash("sha256");
  for await (const chunk of createReadStream(file)) {
    digest.update(chunk);
  }
  return digest.digest("hex");
}

async function main() {
  if (process.argv.length !== 8) {
    die(
      "usage: update-archive.mjs BASE_DIR PLATFORM DESTINATION MAX_MEMBERS MAX_BYTES EXPECTED_SHA256",
    );
  }
  const [
    base,
    platform,
    destination,
    maxMembersText,
    maxBytesText,
    expectedDigest,
  ] = process.argv.slice(2);
  if (!/^[0-9a-f]{64}$/.test(expectedDigest)) {
    die("published archive SHA-256 is malformed");
  }
  if (!/^[1-9][0-9]*$/.test(maxMembersText) || !/^[1-9][0-9]*$/.test(maxBytesText)) {
    die("archive limits are malformed");
  }
  const maxMembers = Number(maxMembersText);
  const maxBytes = BigInt(maxBytesText);
  if (!Number.isSafeInteger(maxMembers)) {
    die("archive member-count limit is invalid");
  }
  const destinationMetadata = await lstat(destination);
  if (!destinationMetadata.isDirectory() || destinationMetadata.isSymbolicLink()) {
    die("updater extraction directory is unsafe");
  }
  if ((await readdir(destination)).length !== 0) {
    die("updater extraction directory is not empty");
  }

  const candidate = await selectArchive(base, platform);
  const verifiedArchive = await copyAndHash(
    candidate,
    destination,
    expectedDigest,
    maxBytes,
  );
  const limits = { maxBytes, maxMembers };
  const scanned = await scanArchive(verifiedArchive, limits, platform);
  await extractArchive(verifiedArchive, destination, limits, scanned);
  if (await hashFile(verifiedArchive) !== expectedDigest) {
    die("verified release archive changed during extraction");
  }

  const platformBytes = await readFile(
    path.join(destination, "game", "config", "release-platform"),
  );
  const expectedPlatformBytes = Buffer.from(`${platform}\n`, "ascii");
  if (!platformBytes.equals(expectedPlatformBytes)) {
    die(
      `archive platform marker does not exactly encode ${JSON.stringify(platform)}`,
    );
  }
  const versionBytes = await readFile(
    path.join(destination, "game", "config", "release-version"),
  );
  const expectedVersionBytes = Buffer.from(`${candidate.version}\n`, "ascii");
  if (!versionBytes.equals(expectedVersionBytes)) {
    die(
      `archive version marker does not exactly encode ${JSON.stringify(candidate.version)}`,
    );
  }
  const marker = scanned.byName.get(`game/current_${candidate.version}`);
  if (!marker || marker.kind !== "file" || marker.size !== 0) {
    die(`archive lacks the authenticated current_${candidate.version} marker`);
  }
  process.stdout.write(`${candidate.version}\t${candidate.name}\n`);
}

main().catch((error) => {
  process.stderr.write(`update-archive: ${error.message}\n`);
  process.exitCode = 1;
});
