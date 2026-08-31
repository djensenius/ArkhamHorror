{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | The optional, additive locale-catalog pointer that
@GET \/api\/v1\/capabilities@ advertises.

The backend never holds, proxies, or re-encodes catalog content: story text
crosses the wire as opaque @I18nEntry@ keys, and the catalog itself is
deployment-owned static JSON (see @docs\/locale-catalog.md@). All this module
adds is the small, closed metadata block a native client needs to bind that
static resource to /this exact server profile/ — where the manifest is, which
revision and schema version it speaks, which locales it carries, and the
SHA-256 the manifest bytes must hash to.

Two deployment shapes are supported, and exactly one authority is published
for either of them (there is deliberately no separate origin/base-path field
that could disagree with the manifest URL):

* __hosted / same-origin__ — a root-relative path such as
  @\/locale-catalog\/manifest.json@, resolved by the client against the origin
  it just called @\/api\/v1\/capabilities@ on. This is the preferred form: it
  cannot point a client at a third party, and it works for any hostname
  without reconfiguration.
* __split / static deployment__ — an explicitly configured absolute @https@
  URL. Nothing else is accepted: no @http@, no scheme-relative @\/\/host\/…@,
  no credentials, no query, no fragment, no percent-encoding, no dot
  segments, no platform paths.

URL handling here is intentionally *not* delegated to a permissive generic URL
parser. A general parser's job is to accept and normalize the widest possible
input; this one's job is the opposite — to accept exactly the two spellings
above, in exactly one normal form each, and to reject everything else before
the value can ever reach a client.

Nothing in this module is advertised unless every field validated, and a
supplied-but-invalid configuration is a startup failure rather than a silent
downgrade to the legacy response (see 'parseLocaleCatalogConfig').
-}
module Base.Api.Types.LocaleCatalog (
  LocaleCatalog (..),
  LocaleCatalogConfig (..),
  LocaleCatalogConfigError (..),
  LocaleCatalogSetting (..),
  ManifestUrlRejection (..),
  localeCatalogCapability,
  localeCatalogMaxSupportedLocales,
  localeCatalogSettingEnvVar,
  localeCatalogSettingKey,
  localeCatalogSupportedSchemaVersions,
  parseLocaleCatalogConfig,
  parseLocaleCatalogSetting,
  parseManifestUrl,
  renderLocaleCatalogConfigError,
) where

import Data.Aeson (Object, ToJSON (..), Value (..), defaultOptions, genericToEncoding, genericToJSON, (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Text qualified as T
import Relude

-- | Capability identifier advertised exactly when 'LocaleCatalog' is present.
localeCatalogCapability :: Text
localeCatalogCapability = "i18n.locale-catalog.v1"

{- | Catalog manifest @schemaVersion@ values this server knows how to describe.
A deployment that publishes a newer catalog schema must not be advertised
through a @v1@ capability, so an unknown value fails startup instead.
-}
localeCatalogSupportedSchemaVersions :: [Text]
localeCatalogSupportedSchemaVersions = ["1.0.0"]

{- | Upper bound on @supportedLocales@, matching both the published catalog
manifest's own @locales@ bound and @capabilities.schema.json@. Without it a
deployment could start happily and then serve a response that fails the
schema every client is told to validate against, which would cost that client
capability negotiation entirely rather than just this optional field.
-}
localeCatalogMaxSupportedLocales :: Int
localeCatalogMaxSupportedLocales = 64

{- | The wire object. Field names mirror the published v1 catalog manifest
(@frontend\/schemas\/locale-catalog\/v1\/manifest.schema.json@) so a client can
compare them without a translation table: @catalogRevision@, @schemaVersion@
and @defaultLocale@ are the manifest's own field names, @manifestUrl@ is the
one authority for @manifestPath@, @supportedLocales@ is the manifest's
@locales[].locale@ set, and @manifestSha256@ pins the manifest bytes with the
manifest's own @digestAlgorithm@.
-}
data LocaleCatalog = LocaleCatalog
  { manifestUrl :: Text
  , catalogRevision :: Text
  , schemaVersion :: Text
  , defaultLocale :: Text
  , supportedLocales :: [Text]
  , manifestSha256 :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Both directions are generated from one 'Data.Aeson.Options' value, so the
-- @toJSON@ a test asserts against and the @toEncoding@ the wire actually uses
-- cannot drift apart.
instance ToJSON LocaleCatalog where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

{- | One configuration setting, so a diagnostic can name the exact key and
environment variable an operator has to fix.
-}
data LocaleCatalogSetting
  = ManifestUrlSetting
  | CatalogRevisionSetting
  | SchemaVersionSetting
  | DefaultLocaleSetting
  | SupportedLocalesSetting
  | ManifestSha256Setting
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | @config\/settings.yml@ key for a setting.
localeCatalogSettingKey :: LocaleCatalogSetting -> Text
localeCatalogSettingKey = \case
  ManifestUrlSetting -> "locale-catalog-manifest-url"
  CatalogRevisionSetting -> "locale-catalog-revision"
  SchemaVersionSetting -> "locale-catalog-schema-version"
  DefaultLocaleSetting -> "locale-catalog-default-locale"
  SupportedLocalesSetting -> "locale-catalog-locales"
  ManifestSha256Setting -> "locale-catalog-manifest-sha256"

-- | Environment variable that overrides a setting at runtime.
localeCatalogSettingEnvVar :: LocaleCatalogSetting -> Text
localeCatalogSettingEnvVar = \case
  ManifestUrlSetting -> "ARKHAM_LOCALE_CATALOG_MANIFEST_URL"
  CatalogRevisionSetting -> "ARKHAM_LOCALE_CATALOG_REVISION"
  SchemaVersionSetting -> "ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION"
  DefaultLocaleSetting -> "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE"
  SupportedLocalesSetting -> "ARKHAM_LOCALE_CATALOG_LOCALES"
  ManifestSha256Setting -> "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"

{- | Read one raw setting out of the settings object.

Values reach this parser from @config\/settings.yml@ after environment
substitution, and @Data.Yaml.Config@ re-parses each substituted value as a YAML
scalar — so a digest that happens to be all decimal digits, or a version typed
as @1.0@, arrives as a @Number@ rather than a @String@. Coercing that back to
text would be lossy and ambiguous (@1.0@, @1e5@), so it is refused, with a
message naming the setting and its environment variable instead of aeson's bare
\"expected String\".
-}
parseLocaleCatalogSetting :: Object -> LocaleCatalogSetting -> Parser (Maybe Text)
parseLocaleCatalogSetting o setting =
  o .:? Key.fromText (localeCatalogSettingKey setting) >>= \case
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just (String value) -> pure (Just value)
    Just _ ->
      fail
        $ toString
        $ "locale catalog configuration is invalid: "
        <> localeCatalogSettingKey setting
        <> " ("
        <> localeCatalogSettingEnvVar setting
        <> ") must be a quoted string; YAML read it as a number or boolean, which happens when a"
        <> " value such as a digest of only digits, or a version like 1.0, is left unquoted"

{- | Raw, untrusted configuration exactly as it arrives from
@config\/settings.yml@ (and therefore from the environment). A key that is
absent, null, or blank is treated as not supplied.
-}
data LocaleCatalogConfig = LocaleCatalogConfig
  { manifestUrl :: Maybe Text
  , catalogRevision :: Maybe Text
  , schemaVersion :: Maybe Text
  , defaultLocale :: Maybe Text
  , supportedLocales :: Maybe Text
  , manifestSha256 :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Every supplied setting, validated, before any of it can be advertised.
data SuppliedLocaleCatalog = SuppliedLocaleCatalog
  { manifestUrl :: Text
  , catalogRevision :: Text
  , schemaVersion :: Text
  , defaultLocale :: Text
  , supportedLocales :: Text
  , manifestSha256 :: Text
  }

{- | Why a manifest URL was refused. Deliberately closed, and deliberately
value-free: a rejected URL is never echoed back into a log line, because the
one thing an operator is most likely to paste in by mistake is a URL carrying
credentials.
-}
data ManifestUrlRejection
  = ManifestUrlTooLong
  | ManifestUrlControlCharacter
  | ManifestUrlNonAscii
  | ManifestUrlWhitespace
  | ManifestUrlBackslash
  | ManifestUrlFragment
  | ManifestUrlQuery
  | ManifestUrlPercentEncoded
  | ManifestUrlSchemeRelative
  | ManifestUrlInsecureScheme
  | ManifestUrlUnsupportedScheme
  | ManifestUrlPlatformPath
  | ManifestUrlNotAbsolute
  | ManifestUrlCredentials
  | ManifestUrlAddressLiteralHost
  | ManifestUrlAmbiguousNumericHost
  | ManifestUrlInvalidHost
  | ManifestUrlInvalidPort
  | ManifestUrlNotAbsolutePath
  | ManifestUrlEmptyPathSegment
  | ManifestUrlDotSegment
  | ManifestUrlInvalidPathCharacter
  | ManifestUrlNotJsonPath
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Why a configuration was refused. Closed, and carries only sanitized,
-- bounded excerpts (see 'excerpt').
data LocaleCatalogConfigError
  = PartiallyConfigured [LocaleCatalogSetting]
  | InvalidManifestUrl ManifestUrlRejection
  | InvalidCatalogRevision Text
  | UnsupportedSchemaVersion Text
  | CatalogRevisionSchemaMismatch Text Text
  | InvalidManifestSha256 Text
  | InvalidLocaleTag LocaleCatalogSetting Text
  | DuplicateLocaleTag Text
  | TooManySupportedLocales Int
  | DefaultLocaleNotSupported Text
  deriving stock (Eq, Show)

{- | Validate the whole configuration up front.

@Right Nothing@ means the catalog is not configured: the capability string and
the @localeCatalog@ object are both omitted, so the response keeps the exact
legacy field and capability shape (@schemaRevision@ still reports this server's
real contract revision — see "Base.Api.Types.Capabilities"). That happens only
when /every/ setting is absent or blank —
blanking some but not all is 'PartiallyConfigured', i.e. a startup failure, so
a half-removed pointer can never be silently dropped and a supplied-but-invalid
configuration can never degrade into "just don't advertise it".
-}
parseLocaleCatalogConfig
  :: LocaleCatalogConfig -> Either LocaleCatalogConfigError (Maybe LocaleCatalog)
parseLocaleCatalogConfig config
  | null suppliedSettings = Right Nothing
  | otherwise = case supplied of
      Nothing -> Left $ PartiallyConfigured [s | (s, Nothing) <- settings]
      Just fields -> Just <$> buildLocaleCatalog fields
 where
  settings :: [(LocaleCatalogSetting, Maybe Text)]
  settings =
    [ (ManifestUrlSetting, present config.manifestUrl)
    , (CatalogRevisionSetting, present config.catalogRevision)
    , (SchemaVersionSetting, present config.schemaVersion)
    , (DefaultLocaleSetting, present config.defaultLocale)
    , (SupportedLocalesSetting, present config.supportedLocales)
    , (ManifestSha256Setting, present config.manifestSha256)
    ]

  suppliedSettings :: [LocaleCatalogSetting]
  suppliedSettings = [s | (s, Just _) <- settings]

  supplied :: Maybe SuppliedLocaleCatalog
  supplied =
    SuppliedLocaleCatalog
      <$> present config.manifestUrl
      <*> present config.catalogRevision
      <*> present config.schemaVersion
      <*> present config.defaultLocale
      <*> present config.supportedLocales
      <*> present config.manifestSha256

  present :: Maybe Text -> Maybe Text
  present raw = case T.strip <$> raw of
    Just value | not (T.null value) -> Just value
    _ -> Nothing

buildLocaleCatalog :: SuppliedLocaleCatalog -> Either LocaleCatalogConfigError LocaleCatalog
buildLocaleCatalog fields = do
  manifestUrl <- first InvalidManifestUrl $ parseManifestUrl fields.manifestUrl
  schemaVersion <- parseSchemaVersion fields.schemaVersion
  catalogRevision <- parseCatalogRevision schemaVersion fields.catalogRevision
  manifestSha256 <- parseSha256 fields.manifestSha256
  defaultLocale <- parseLocaleTag DefaultLocaleSetting fields.defaultLocale
  supportedLocales <- parseSupportedLocales fields.supportedLocales
  unless (defaultLocale `elem` supportedLocales)
    $ Left (DefaultLocaleNotSupported defaultLocale)
  pure LocaleCatalog {..}

-- * Catalog revision, schema version, digest

parseSchemaVersion :: Text -> Either LocaleCatalogConfigError Text
parseSchemaVersion raw
  | raw `elem` localeCatalogSupportedSchemaVersions = Right raw
  | otherwise = Left $ UnsupportedSchemaVersion (excerpt raw)

{- | A catalog revision is the manifest's own @catalogRevision@: a major
component matching the catalog schema's major version, then a dot, then the
32 lowercase hex characters the generator derives from the provenance digest.
The cross-check against 'parseSchemaVersion''s result is what makes a
@1.…@ revision paired with a future schema version an inconsistency rather
than something a client has to notice for itself.
-}
parseCatalogRevision :: Text -> Text -> Either LocaleCatalogConfigError Text
parseCatalogRevision schemaVersion raw = case T.breakOn "." raw of
  (major, rest)
    | Just digest <- T.stripPrefix "." rest
    , isCanonicalNumber major
    , T.length digest == 32
    , T.all isLowerHexDigit digest ->
        if major == T.takeWhile Char.isDigit schemaVersion
          then Right raw
          else Left $ CatalogRevisionSchemaMismatch (excerpt raw) schemaVersion
  _ -> Left $ InvalidCatalogRevision (excerpt raw)

parseSha256 :: Text -> Either LocaleCatalogConfigError Text
parseSha256 raw
  | T.length raw == 64, T.all isLowerHexDigit raw = Right raw
  | otherwise = Left $ InvalidManifestSha256 (excerpt raw)

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c = Char.isDigit c || (c >= 'a' && c <= 'f')

isCanonicalNumber :: Text -> Bool
isCanonicalNumber t =
  not (T.null t) && T.all Char.isDigit t && (t == "0" || not ("0" `T.isPrefixOf` t))

-- * Locales

{- | Supported locales arrive as one comma-separated list, are canonicalized,
de-duplicated /after/ canonicalization (so @en-us@ and @en-US@ collide), and
are published in ascending order so the response bytes do not depend on the
order an operator happened to type.
-}
parseSupportedLocales :: Text -> Either LocaleCatalogConfigError [Text]
parseSupportedLocales raw = do
  let entries = T.splitOn "," raw
  when (length entries > localeCatalogMaxSupportedLocales)
    $ Left (TooManySupportedLocales (length entries))
  locales <- traverse (parseLocaleTag SupportedLocalesSetting) entries
  let ordered = List.sort locales
  case listToMaybe [a | (a, b) <- zip ordered (drop 1 ordered), a == b] of
    Just duplicate -> Left $ DuplicateLocaleTag duplicate
    Nothing -> Right ordered

parseLocaleTag :: LocaleCatalogSetting -> Text -> Either LocaleCatalogConfigError Text
parseLocaleTag setting raw =
  maybe (Left $ InvalidLocaleTag setting (excerpt raw)) Right (canonicalLocaleTag (T.strip raw))

{- | Accept the catalog manifest's locale grammar (a 2–3 letter primary subtag
followed by 2–8 alphanumeric subtags) in any case, and return the BCP-47
canonical spelling: lowercase language, titlecase 4-letter script, uppercase
2-letter region, lowercase everything else.
-}
canonicalLocaleTag :: Text -> Maybe Text
canonicalLocaleTag raw = do
  guard $ not (T.null raw) && T.length raw <= 64
  (primary, subtags) <- case T.splitOn "-" raw of
    [] -> Nothing
    p : rest -> Just (p, rest)
  guard $ T.length primary >= 2 && T.length primary <= 3 && T.all isAsciiAlpha primary
  guard $ all isSubtag subtags
  pure $ T.intercalate "-" (T.toLower primary : map canonicalSubtag subtags)
 where
  isSubtag t = T.length t >= 2 && T.length t <= 8 && T.all isAsciiAlphaNum t

  canonicalSubtag t
    | T.length t == 4, T.all isAsciiAlpha t = titlecase t
    | T.length t == 2, T.all isAsciiAlpha t = T.toUpper t
    | otherwise = T.toLower t

  titlecase t = case T.uncons t of
    Nothing -> t
    Just (c, rest) -> T.cons (Char.toUpper c) (T.toLower rest)

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = Char.isAsciiLower c || Char.isAsciiUpper c

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || Char.isDigit c

-- * Manifest URL

{- | Accept a root-relative path or an absolute @https@ URL, and return the one
normal form of whichever was given (lowercased scheme and host, redundant
@:443@ removed). Every rejection below is a deliberate refusal rather than a
gap: the checks run before any structural parsing, so no later step ever has to
reason about a control character, a backslash, a percent escape, or a query.
-}
parseManifestUrl :: Text -> Either ManifestUrlRejection Text
parseManifestUrl raw
  | T.length raw > 512 = Left ManifestUrlTooLong
  | T.any isControlChar raw = Left ManifestUrlControlCharacter
  | T.any (not . Char.isAscii) raw = Left ManifestUrlNonAscii
  | T.any (== ' ') raw = Left ManifestUrlWhitespace
  | T.any (== '\\') raw = Left ManifestUrlBackslash
  | T.any (== '#') raw = Left ManifestUrlFragment
  | T.any (== '?') raw = Left ManifestUrlQuery
  | T.any (== '%') raw = Left ManifestUrlPercentEncoded
  | "//" `T.isPrefixOf` raw = Left ManifestUrlSchemeRelative
  | "/" `T.isPrefixOf` raw = raw <$ validateManifestPath raw
  | Just rest <- stripSchemeCI "https://" raw = parseHttpsUrl rest
  | isJust (stripSchemeCI "http://" raw) = Left ManifestUrlInsecureScheme
  | isPlatformPath raw = Left ManifestUrlPlatformPath
  | hasScheme raw = Left ManifestUrlUnsupportedScheme
  | otherwise = Left ManifestUrlNotAbsolute
 where
  isControlChar c = c < ' ' || c == '\DEL'

parseHttpsUrl :: Text -> Either ManifestUrlRejection Text
parseHttpsUrl rest = do
  let (authority, path) = T.break (== '/') rest
  when (T.any (== '@') authority) $ Left ManifestUrlCredentials
  when (T.any (\c -> c == '[' || c == ']') authority) $ Left ManifestUrlAddressLiteralHost
  when (T.null path) $ Left ManifestUrlNotAbsolutePath
  validateManifestPath path
  let (rawHost, rawPort) = T.breakOn ":" authority
  host <- validateHost rawHost
  port <- validatePort rawPort
  pure $ "https://" <> host <> port <> path

{- | Bind the authority to a host every client resolves identically.

Lowercase-normalized, dot-separated LDH labels, and then exactly one of:

* __canonical dotted-decimal IPv4__ — four decimal octets, each @0-255@, with
  no leading-zero padding; or
* __a registered DNS name that no URL parser will reinterpret as an address__.

The second condition is the one that matters, and it is not merely
cosmetic. WHATWG's host parser (and @inet_aton@ before it) treats a host whose
final label is a number as IPv4 and accepts short, hex and octal forms, so
@127.1@, @2130706433@, @0x7f000001@ and @01.02.03.04@ all mean @127.0.0.1@ —
while a strict RFC 3986 reader, a Haskell client, and a browser can each
disagree about @999.999@ or @1.2.3.4.5@. A manifest URL that means different
things to different clients is exactly what this contract must never publish,
so anything that is not unambiguously one of the two forms above is refused at
startup rather than emitted.

Address literals (@[::1]@, @[v7.x]@) are refused by the caller: there is no
second host syntax to normalize here, so a bracketed literal can never reach a
client in a non-canonical spelling.
-}
validateHost :: Text -> Either ManifestUrlRejection Text
validateHost rawHost = do
  when (T.null rawHost || T.length rawHost > 253) $ Left ManifestUrlInvalidHost
  unless (all isLabel labels) $ Left ManifestUrlInvalidHost
  lastLabel <- maybe (Left ManifestUrlInvalidHost) Right (viaNonEmpty last labels)
  if isCanonicalIpv4Host labels
    then Right host
    else do
      when (readsAsNumericHost lastLabel) $ Left ManifestUrlAmbiguousNumericHost
      Right host
 where
  host = T.toLower rawHost
  labels = T.splitOn "." host
  isLabel label =
    not (T.null label)
      && T.length label <= 63
      && T.all (\c -> Char.isAsciiLower c || Char.isDigit c || c == '-') label
      && not ("-" `T.isPrefixOf` label)
      && not ("-" `T.isSuffixOf` label)

-- | Exactly four decimal octets in @0-255@, spelled without leading zeros.
isCanonicalIpv4Host :: [Text] -> Bool
isCanonicalIpv4Host labels = length labels == 4 && all isCanonicalOctet labels
 where
  isCanonicalOctet label =
    isCanonicalNumber label
      && T.length label <= 3
      && maybe False (<= 255) (readMaybe (toString label) :: Maybe Int)

{- | WHATWG's \"ends in a number\" test, which is what decides whether a host
is handed to the IPv4 parser at all: a final label that is all decimal digits,
or an @0x@-prefixed hex literal (including a bare @0x@, which is zero).
-}
readsAsNumericHost :: Text -> Bool
readsAsNumericHost label =
  T.all Char.isDigit label
    || maybe False (T.all isLowerHexDigit) (T.stripPrefix "0x" label)

-- | An explicit @:443@ is dropped rather than published, so the same
-- deployment cannot be described by two different strings.
validatePort :: Text -> Either ManifestUrlRejection Text
validatePort rawPort = case T.stripPrefix ":" rawPort of
  Nothing -> Right ""
  Just digits
    | isCanonicalNumber digits
    , T.length digits <= 5
    , Just port <- readMaybe (toString digits)
    , port >= (1 :: Int)
    , port <= 65535 ->
        Right $ if port == 443 then "" else ":" <> digits
  Just _ -> Left ManifestUrlInvalidPort

validateManifestPath :: Text -> Either ManifestUrlRejection ()
validateManifestPath path
  | not ("/" `T.isPrefixOf` path) = Left ManifestUrlNotAbsolutePath
  | "//" `T.isInfixOf` path || "/" `T.isSuffixOf` path = Left ManifestUrlEmptyPathSegment
  | any (\segment -> segment == "." || segment == "..") segments = Left ManifestUrlDotSegment
  | not (T.all isPathChar path) = Left ManifestUrlInvalidPathCharacter
  | not (".json" `T.isSuffixOf` path) = Left ManifestUrlNotJsonPath
  | otherwise = Right ()
 where
  segments = drop 1 (T.splitOn "/" path)
  isPathChar c = isAsciiAlphaNum c || c == '-' || c == '.' || c == '_' || c == '~' || c == '/'

stripSchemeCI :: Text -> Text -> Maybe Text
stripSchemeCI scheme raw
  | T.toLower (T.take (T.length scheme) raw) == scheme = Just $ T.drop (T.length scheme) raw
  | otherwise = Nothing

-- | @C:\/…@ and friends, checked before 'hasScheme' because a drive letter
-- also looks like a one-character scheme.
isPlatformPath :: Text -> Bool
isPlatformPath raw = case T.unpack (T.take 2 raw) of
  [drive, ':'] -> isAsciiAlpha drive
  _ -> False

hasScheme :: Text -> Bool
hasScheme raw = case T.break (== ':') raw of
  (scheme, rest) -> case T.uncons scheme of
    Just (c, _) -> not (T.null rest) && isAsciiAlpha c && T.all isSchemeChar scheme
    Nothing -> False
 where
  isSchemeChar c = isAsciiAlphaNum c || c == '+' || c == '-' || c == '.'

-- * Diagnostics

{- | A bounded, ASCII-only excerpt of a rejected value. Narrative text and
secrets never reach this module, and a manifest URL is never excerpted at all,
but a configuration diagnostic still ends up in deployment logs — so the value
that is echoed is truncated and stripped of anything that could forge a log
line.
-}
excerpt :: Text -> Text
excerpt = T.map keep . T.take 32
 where
  keep c
    | isAsciiAlphaNum c || c == '.' || c == '-' || c == '_' = c
    | otherwise = '?'

-- | Sanitized, actionable startup diagnostic. Every message names the settings
-- key and environment variable an operator has to change.
renderLocaleCatalogConfigError :: LocaleCatalogConfigError -> Text
renderLocaleCatalogConfigError err = "locale catalog configuration is invalid: " <> case err of
  PartiallyConfigured missing ->
    "the catalog pointer is partially configured; set "
      <> renderSettings missing
      <> ", or clear every locale-catalog setting to disable the catalog pointer entirely"
  InvalidManifestUrl rejection ->
    setting ManifestUrlSetting
      <> " must be a same-origin absolute path such as /locale-catalog/manifest.json, or an"
      <> " absolute https URL; it was rejected because "
      <> renderManifestUrlRejection rejection
      <> " (the value itself is not logged, because a manifest URL can carry credentials)"
  InvalidCatalogRevision value ->
    setting CatalogRevisionSetting
      <> " must be a catalog revision such as 1.<32 lowercase hex characters>, got '"
      <> value
      <> "'"
  UnsupportedSchemaVersion value ->
    setting SchemaVersionSetting
      <> " must be one of "
      <> T.intercalate ", " localeCatalogSupportedSchemaVersions
      <> ", got '"
      <> value
      <> "'"
  CatalogRevisionSchemaMismatch value schemaVersion ->
    setting CatalogRevisionSetting
      <> " ('"
      <> value
      <> "') does not belong to catalog schema version "
      <> schemaVersion
      <> "; its major component must match the schema's major version"
  InvalidManifestSha256 value ->
    setting ManifestSha256Setting
      <> " must be 64 lowercase hex characters, got '"
      <> value
      <> "'"
  InvalidLocaleTag setting' value ->
    setting setting'
      <> " contains an invalid locale tag '"
      <> value
      <> "'; expected a catalog locale such as en, pt-BR or zh-Hant"
  TooManySupportedLocales count ->
    setting SupportedLocalesSetting
      <> " lists "
      <> show count
      <> " locales, but a catalog can publish at most "
      <> show localeCatalogMaxSupportedLocales
  DuplicateLocaleTag locale ->
    setting SupportedLocalesSetting
      <> " lists '"
      <> locale
      <> "' more than once (locale tags are compared in canonical form)"
  DefaultLocaleNotSupported locale ->
    setting DefaultLocaleSetting
      <> " ('"
      <> locale
      <> "') is not listed in "
      <> setting SupportedLocalesSetting
 where
  setting s = localeCatalogSettingKey s <> " (" <> localeCatalogSettingEnvVar s <> ")"
  renderSettings = T.intercalate ", " . map setting . List.sort

renderManifestUrlRejection :: ManifestUrlRejection -> Text
renderManifestUrlRejection = \case
  ManifestUrlTooLong -> "it is longer than 512 characters"
  ManifestUrlControlCharacter -> "it contains a control character"
  ManifestUrlNonAscii -> "it contains a non-ASCII character"
  ManifestUrlWhitespace -> "it contains a space"
  ManifestUrlBackslash -> "it contains a backslash"
  ManifestUrlFragment -> "it contains a fragment"
  ManifestUrlQuery -> "it contains a query string"
  ManifestUrlPercentEncoded -> "it contains a percent escape"
  ManifestUrlSchemeRelative -> "it is scheme-relative"
  ManifestUrlInsecureScheme -> "http is not accepted, only https"
  ManifestUrlUnsupportedScheme -> "its scheme is not https"
  ManifestUrlPlatformPath -> "it is a platform file path, not a URL"
  ManifestUrlNotAbsolute -> "it is neither an absolute path nor an absolute https URL"
  ManifestUrlCredentials -> "it carries credentials in its authority"
  ManifestUrlAddressLiteralHost ->
    "its host is a bracketed address literal, which this contract does not publish"
  ManifestUrlAmbiguousNumericHost ->
    "its host is a number that URL parsers disagree about; use canonical dotted-decimal"
      <> " IPv4 (four octets, 0-255, no leading zeros) or a name whose last label is not numeric"
  ManifestUrlInvalidHost -> "its host is not a valid registered name"
  ManifestUrlInvalidPort -> "its port is not a number between 1 and 65535"
  ManifestUrlNotAbsolutePath -> "its path is missing or not absolute"
  ManifestUrlEmptyPathSegment -> "its path has an empty segment or a trailing slash"
  ManifestUrlDotSegment -> "its path contains a '.' or '..' segment"
  ManifestUrlInvalidPathCharacter -> "its path contains an unsupported character"
  ManifestUrlNotJsonPath -> "its path does not end in .json"
