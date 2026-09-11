#!/usr/bin/env python3
import copy
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[5]
EXE = Path(sys.argv[1]).resolve()
EXPECT_UNATTESTED = sys.argv[2:] == ["--expect-unattested"]
FIXTURE = ROOT / "backend/arkham-api/tests/fixtures/replay/smoke-current-data.json"


def invoke(*args, error=None):
    result = subprocess.run([EXE, *map(str, args)], text=True, capture_output=True)
    if error is None and result.returncode:
        raise AssertionError(f"command failed ({result.returncode}): {result.stderr}")
    if error is not None and (not result.returncode or error not in result.stderr):
        raise AssertionError(f"expected failure containing {error!r}: {result.stderr}")
    return result


def write(path, value):
    path.write_text(json.dumps(value, separators=(",", ":"), sort_keys=True))


def bind_source(template, source):
    plan = copy.deepcopy(template)
    plan["source"] = source
    return plan


def reject(work, source_path, plan, name, error, options=()):
    plan_path, output = work / f"{name}.plan.json", work / f"{name}.checkpoint.json"
    write(plan_path, plan)
    invoke(
        source_path,
        *options,
        "--replay-script",
        plan_path,
        "--checkpoint-output",
        output,
        error=error,
    )
    assert not output.exists(), f"{name} published a checkpoint"


def main():
    fixture = json.loads(FIXTURE.read_text())
    source, template = fixture["source"], fixture["plan"]
    with tempfile.TemporaryDirectory(prefix=".replay-process-", dir=ROOT) as tmp:
        work = Path(tmp)
        source_path = work / "source.json"
        write(source_path, source)

        current = json.loads(invoke(source_path, "--inspect-checkpoint").stdout)
        undone = json.loads(invoke(source_path, "--undo", 1, "--inspect-checkpoint").stdout)
        assert current["questions"] == [template["stopAt"]]
        assert undone["questions"] == [template["answers"][0]["expect"]]
        assert current["source"] == undone["source"]
        answer_plan = bind_source(template, undone["source"])

        if EXPECT_UNATTESTED:
            assert undone["source"]["replayBuild"]["attestation"] == "unattested"
            reject(
                work,
                source_path,
                answer_plan,
                "unattested",
                "no build attestation",
                ("--undo", 1),
            )
            print("replay process fixture: unattested executable rejected")
            return

        assert undone["source"]["replayBuild"]["attestation"] != "unattested"
        predrain_plan = bind_source(template, current["source"])
        predrain_plan["answers"] = []
        predrain_plan_path = work / "predrain.plan.json"
        predrain_output = work / "predrain.checkpoint.json"
        write(predrain_plan_path, predrain_plan)
        invoke(
            source_path,
            "--undo",
            0,
            "--replay-script",
            predrain_plan_path,
            "--checkpoint-output",
            predrain_output,
        )
        predrain = json.loads(predrain_output.read_text())
        predrain_provenance = predrain["replayCheckpoint"]["provenance"]
        assert predrain_provenance["answersApplied"] == 0
        assert predrain_provenance["undoSteps"] == 0
        assert predrain["campaignData"]["steps"][0]["choice"]["choiceMessages"] == [{"tag": "Noop"}]

        plan_path = work / "answer.plan.json"
        first, second = work / "first.checkpoint.json", work / "second.checkpoint.json"
        metrics = work / "metrics.txt"
        write(plan_path, answer_plan)
        traced = invoke(
            source_path,
            "--undo",
            1,
            "--trace",
            "--replay-script",
            plan_path,
            "--checkpoint-output",
            first,
        )
        trace_lines = [line for line in traced.stderr.splitlines() if line.startswith("> ")]
        clear_index = trace_lines.index("> ClearUI")
        ask_index = next(i for i, line in enumerate(trace_lines) if line.startswith("> Ask "))
        assert clear_index < ask_index, trace_lines
        assert "> Noop" not in trace_lines, trace_lines
        invoke(
            source_path,
            "--undo",
            1,
            "--simulate-server",
            "--metrics",
            metrics,
            "--replay-script",
            plan_path,
            "--checkpoint-output",
            second,
        )
        assert first.read_bytes() == second.read_bytes(), "post-answer checkpoint bytes differ"
        metric_text = metrics.read_text()
        for span in (
            "server/forceActionDiff",
            "server/diffDown",
            "server/encodeGame",
            "server/parseGame",
            "server/encodePublicGame",
        ):
            assert span in metric_text, f"missing simulate-server span {span}"

        exported = json.loads(first.read_text())
        data = exported["campaignData"]
        provenance = exported["replayCheckpoint"]["provenance"]
        assert data["currentData"] == source["campaignData"]["currentData"]
        assert data["step"] == 0 and data["log"] == []
        assert data["steps"][0]["choice"]["choiceMessages"] == [{"tag": "Noop"}]
        assert provenance["undoSteps"] == 1 and provenance["answersApplied"] == 1
        assert provenance["checkpoint"] == template["stopAt"]
        assert provenance["replayBuild"] == undone["source"]["replayBuild"]

        checked = json.loads(invoke(first, "--inspect-checkpoint").stdout)
        assert checked["source"]["inputKind"] == "checkpoint"
        assert checked["inputProvenance"] == provenance
        imported = work / "imported-game.json"
        invoke(first, "--output", imported)
        assert json.loads(imported.read_text()) == data["currentData"]

        stale_prompt = copy.deepcopy(answer_plan)
        stale_prompt["answers"][0]["expect"]["promptSha256"] = "0" * 64
        reject(work, source_path, stale_prompt, "prompt", "promptSha256 mismatch", ("--undo", 1))
        stale_version = copy.deepcopy(answer_plan)
        stale_version["answers"][0]["answer"]["contents"]["questionVersion"] = 2
        reject(work, source_path, stale_version, "version", "questionVersion mismatch", ("--undo", 1))
        stale_contract = copy.deepcopy(answer_plan)
        stale_contract["source"]["schemaRevision"] = "0.0.0"
        reject(work, source_path, stale_contract, "contract", "schemaRevision", ("--undo", 1))
        stale_build = copy.deepcopy(answer_plan)
        build_hash = stale_build["source"]["replayBuild"]["sourceSha256"]
        stale_build["source"]["replayBuild"]["sourceSha256"] = (
            ("0" if build_hash[0] != "0" else "1") + build_hash[1:]
        )
        reject(work, source_path, stale_build, "build", "build identity mismatch", ("--undo", 1))

        unused = copy.deepcopy(answer_plan)
        unused["answers"].append(copy.deepcopy(unused["answers"][0]))
        reject(work, source_path, unused, "unused", "still unused", ("--undo", 1))
        incompatible = copy.deepcopy(answer_plan)
        incompatible["answers"][0]["answer"] = {
            "tag": "DeckAnswer",
            "deckId": "00000000-0000-0000-0000-000000000002",
            "playerId": template["answers"][0]["expect"]["playerId"],
        }
        reject(
            work,
            source_path,
            incompatible,
            "incompatible",
            "not compatible with checkpoint prompt",
            ("--undo", 1),
        )
        wrong_destiny = copy.deepcopy(answer_plan)
        wrong_destiny["answers"][0]["answer"] = {
            "tag": "PickDestinyAnswer",
            "contents": [],
        }
        reject(
            work,
            source_path,
            wrong_destiny,
            "wrong-destiny",
            "PickDestinyAnswer is not compatible",
            ("--undo", 1),
        )
        unreachable = copy.deepcopy(answer_plan)
        unreachable["stopAt"]["questionVersion"] = 3
        reject(work, source_path, unreachable, "unreachable", "Stop checkpoint not reached", ("--undo", 1))

        history = copy.deepcopy(answer_plan)
        history["mode"] = "history"
        reject(work, source_path, history, "history", "history", ("--undo", 1))
        reject(
            work,
            source_path,
            answer_plan,
            "replay-all",
            "cannot use --replay-all",
            ("--undo", 1, "--replay-all"),
        )
    print("replay process fixture: pre-drain, post-answer determinism, and rejection matrix passed")


if __name__ == "__main__":
    main()
