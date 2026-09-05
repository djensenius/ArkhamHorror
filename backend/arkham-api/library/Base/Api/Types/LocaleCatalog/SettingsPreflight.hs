{-# LANGUAGE ScopedTypeVariables #-}

{- | One immutable, bounded startup snapshot of the YAML settings sources and
of the process environment.

@yaml-0.11.11.2@'s 'Data.Yaml.Config.loadYamlSettings' decodes every runtime
file with @!include@ support, right-associates a left-biased merge over those
values and the compile-time values, applies one environment map, and then
converts the result. This module reproduces that pipeline exactly, and adds
only locale-catalog rejections on top of it:

* every settings byte source — each command-line file, each transitive
  @!include@, and the embedded default settings — is read exactly once,
  through one open handle and never past the snapshot's remaining byte
  budget, and the raw-event analysis, the @!include@ expansion, the
  structural validation and the decode that produces the final value all
  consume that one copy, so a file that is rewritten, renamed or re-pointed
  after startup began cannot change what this process loaded;
* each @!include@ spelling in a source is resolved exactly once, so every
  occurrence of it — and the capture, the analysis and the load — name the
  same file even while something is retargeting a symlink underneath;
* the process environment is read exactly once, and the same immutable map is
  used both to decide which raw values need checking and by
  @'applyEnvValue' False@;
* every raw mapping is analyzed before YAML resolves merge keys, so a
  noncanonical or malformed @_env:@ mapping for one of the six locale-catalog
  settings is refused wherever it appears — including behind an anchor, an
  alias, an inline or sequence @\<\<@ merge, an @!include@, or a nested value
  under the setting's own key — even when a higher-precedence source overrides
  it;
* every bound is charged while the work it bounds is being done rather than
  after it: a source that is not a regular file is refused before it is read,
  its bytes stop at the remaining budget, its YAML events and collection depth
  are charged as libyaml emits them, and the analysis budget covers building
  the raw nodes as well as walking them. Include depth, include count and
  include-graph traversal are bounded the same way, so a hostile settings tree
  fails fast instead of exhausting the process.

Diagnostics name the setting and its canonical environment variable. A raw
environment value is never included in a message and 'SettingsSnapshot' has no
'Show' instance, so no configured secret can reach a log through this module.
-}
module Base.Api.Types.LocaleCatalog.SettingsPreflight (
  SettingsSnapshot,
  captureSettingsSnapshot,
  captureSettingsSnapshotWithEnvironment,
  loadSettingsSnapshot,
  mergeSettingsValues,
  settingsSnapshotFromValues,
  validateLocaleCatalogSettingsValues,
) where

import Base.Api.Types.LocaleCatalog (
  LocaleCatalogSetting,
  localeCatalogSettingEnvVar,
  localeCatalogSettingKey,
  validateLocaleCatalogRawEnvironmentValue,
 )
import Control.Exception qualified as Exception
import Control.Monad (foldM)
import Control.Monad.State.Strict qualified as State
import Data.Aeson (FromJSON, Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.Conduit (ConduitT, (.|), await, runConduitRes)
import Data.Conduit.List qualified as Conduit
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Yaml qualified as Yaml
import Data.Yaml.Config (applyEnvValue)
import Data.Yaml.Internal qualified as YamlInternal
import Relude
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, hFileSize, openBinaryFile)
import Text.Libyaml qualified as Libyaml

{- | Everything startup is allowed to depend on: the settings values decoded
from the captured bytes, in @loadYamlSettings@ order, and the one environment
map that will be applied to their merge.
-}
data SettingsSnapshot = SettingsSnapshot
  { snapshotEnvironment :: !(KeyMap Text)
  , snapshotValues :: ![Value]
  }

-- | One settings file, read once.
data CapturedSource = CapturedSource
  { sourceEvents :: ![Libyaml.Event]
  , sourceIncludes :: !(Map ByteString FilePath)
  }

data CaptureState = CaptureState
  { capturedSources :: !(Map FilePath CapturedSource)
  , capturedBytes :: !Int
  , capturedSteps :: !Int
  }

{- | A YAML node with anchors already resolved the way @yaml@ resolves them:
an anchor is defined only once its node is complete, a later anchor of the
same name replaces an earlier one, and an alias stands for whatever its
anchor named at that point in the stream.
-}
data RawNode
  = RawScalar !ByteString
  | RawSequence ![RawNode]
  | RawMapping ![(RawNode, RawNode)]

{- | Materializing the raw nodes is itself charged against the analysis budget,
so a source is refused while it is being turned into nodes rather than after.
-}
data RawParseState = RawParseState
  { rawAnchors :: !(Map Libyaml.AnchorName RawNode)
  , rawBudget :: !Int
  }

type RawParse = State.StateT RawParseState (Either Text)

-- | Bounded analysis of the resolved raw nodes. Aliases are shared, so an
-- alias-expansion bomb is refused instead of being walked.
type Analysis = State.StateT Int (Either Text)

maxIncludeDepth :: Int
maxIncludeDepth = 32

maxSnapshotSources :: Int
maxSnapshotSources = 256

maxSnapshotBytes :: Int
maxSnapshotBytes = 4 * 1024 * 1024

-- | Include graphs may share sources, so depth and count alone do not bound
-- the walk; this bounds the walk itself.
maxIncludeSteps :: Int
maxIncludeSteps = 4096

-- | Events one source may emit, charged as the parser emits them.
maxSourceEvents :: Int
maxSourceEvents = 128 * 1024

-- | Collection nesting, charged as the parser emits it and again when the
-- include-expanded stream is turned into nodes.
maxNodeDepth :: Int
maxNodeDepth = 256

{- | Events the whole snapshot may expand to, across every runtime root.

@!include@ multiplies a target's events per occurrence, so this bounds the
multiplication rather than the sources: two hundred and fifty-six thousand
events is a settings tree far larger than any deployment's, and well past the
largest this repository's own checks build.
-}
maxExpandedEvents :: Int
maxExpandedEvents = 256 * 1024

-- | Steps the whole snapshot may spend building and walking raw nodes.
-- Aliases are shared, so this has more headroom than the event bound.
maxAnalysisSteps :: Int
maxAnalysisSteps = 1024 * 1024

invalidConfiguration :: Text
invalidConfiguration = "locale catalog configuration is invalid"

invalidYaml :: Text
invalidYaml = withInvalidPrefix "unable to parse a settings YAML source"

unreadableSource :: Text
unreadableSource = withInvalidPrefix "unable to read a settings source"

nonRegularSource :: Text
nonRegularSource = withInvalidPrefix "a settings source is not a regular file"

byteLimitExceeded :: Text
byteLimitExceeded = withInvalidPrefix "settings sources exceed the configured byte limit"

eventLimitExceeded :: Text
eventLimitExceeded = withInvalidPrefix "a settings source exceeds the configured YAML event limit"

depthLimitExceeded :: Text
depthLimitExceeded = withInvalidPrefix "settings YAML nests deeper than the configured limit"

analysisLimitExceeded :: Text
analysisLimitExceeded = "settings YAML exceeds the configured analysis limit"

withInvalidPrefix :: Text -> Text
withInvalidPrefix message = invalidConfiguration <> ": " <> message

failSettings :: Text -> IO a
failSettings = fail . toString

-- | Fail on a diagnostic that already names the invalid configuration.
orFail :: Either Text a -> IO a
orFail = either failSettings pure

-- | Fail on a diagnostic that describes only the problem.
orFailInvalid :: Either Text a -> IO a
orFailInvalid = either (failSettings . withInvalidPrefix) pure

{- | Capture the command-line settings files, their transitive @!include@s,
the embedded settings bytes and the complete process environment, once. The
result is the only input 'loadSettingsSnapshot' consumes.
-}
captureSettingsSnapshot :: [FilePath] -> [ByteString] -> IO SettingsSnapshot
captureSettingsSnapshot runtimeFiles embeddedBytes = do
  environment <- environmentMap <$> getEnvironment
  captureSettingsSnapshotWithEnvironment runtimeFiles embeddedBytes environment

{- | The injectable variant, for deterministic callers and focused tests.
Production obtains its environment through 'captureSettingsSnapshot'.
-}
captureSettingsSnapshotWithEnvironment
  :: [FilePath]
  -> [ByteString]
  -> KeyMap Text
  -> IO SettingsSnapshot
captureSettingsSnapshotWithEnvironment runtimeFiles embeddedBytes environment = do
  let embeddedSize = sum $ map ByteString.length embeddedBytes
  when (embeddedSize > maxSnapshotBytes) $ failSettings byteLimitExceeded
  (runtimeRoots, captured) <-
    State.runStateT
      (traverse (captureSourceGraph 0 []) runtimeFiles)
      CaptureState {capturedSources = mempty, capturedBytes = embeddedSize, capturedSteps = maxIncludeSteps}
  -- Naming one file twice is one file. @mergeValues@ is idempotent -- a value
  -- merged over itself is that value, at every object boundary -- so dropping
  -- the later occurrences of a canonical root leaves the merge, and therefore
  -- precedence, exactly as the package would have computed it, while the work
  -- and the budget it would have consumed are spent once.
  runtimeEvents <-
    orFailInvalid $ expandCapturedSources captured.capturedSources (ordNub runtimeRoots)
  embeddedEvents <- traverse parseEvents embeddedBytes
  orFailInvalid $ analyzeCapturedEvents (runtimeEvents <> embeddedEvents)
  runtimeValues <- traverse decodeExpandedValue runtimeEvents
  embeddedValues <- traverse decodeEmbeddedBytes embeddedBytes
  let values = runtimeValues <> embeddedValues
  orFailInvalid $ validateLocaleCatalogSettingsValues values
  for_ (nonEmpty values) \nonEmptyValues ->
    orFail $ validateCanonicalEnvironmentValues environment (mergeNonEmptySettingsValues nonEmptyValues)
  pure SettingsSnapshot {snapshotEnvironment = environment, snapshotValues = values}

{- | Build a no-files snapshot from settings values a caller already owns.
Used by equivalence tests and by programmatic callers; production always goes
through 'captureSettingsSnapshot'.
-}
settingsSnapshotFromValues :: KeyMap Text -> [Value] -> SettingsSnapshot
settingsSnapshotFromValues environment values =
  SettingsSnapshot {snapshotEnvironment = environment, snapshotValues = values}

{- | Apply the captured environment to the captured values exactly as
'Data.Yaml.Config.loadYamlSettings' does, then convert to the settings type.
-}
loadSettingsSnapshot :: FromJSON settings => SettingsSnapshot -> IO settings
loadSettingsSnapshot snapshot =
  case Yaml.parseEither parseJSON resolvedValue of
    Left message -> error $ "Could not convert to expected type: " <> toText message
    Right settings -> pure settings
 where
  resolvedValue =
    applyEnvValue False snapshot.snapshotEnvironment
      $ mergeSettingsValues snapshot.snapshotValues

{- | @sconcat . fmap MergedValue@ from @Data.Yaml.Config@: right-associated,
and left-biased at every object boundary. Any non-object on the left wins
outright, which is what makes an earlier scalar or null hide everything after
it.
-}
mergeSettingsValues :: [Value] -> Value
mergeSettingsValues values =
  case nonEmpty values of
    Nothing -> error "loadYamlSettings: No configuration provided"
    Just present -> mergeNonEmptySettingsValues present

mergeNonEmptySettingsValues :: NonEmpty Value -> Value
mergeNonEmptySettingsValues (value :| rest) =
  case nonEmpty rest of
    Nothing -> value
    Just remaining -> mergeValues value (mergeNonEmptySettingsValues remaining)
 where
  mergeValues (Object left) (Object right) = Object $ KeyMap.unionWith mergeValues left right
  mergeValues left _ = left

environmentMap :: [(String, String)] -> KeyMap Text
environmentMap = KeyMap.fromList . map (bimap (Key.fromText . toText) toText)

{- | Read one settings file and every file it transitively includes, applying
the same ancestor-cycle rule @Data.Yaml.Include@ uses. Returns the canonical
path the captured bytes were read from.
-}
captureSourceGraph
  :: Int
  -> [FilePath]
  -> FilePath
  -> State.StateT CaptureState IO FilePath
captureSourceGraph depth ancestors requestedPath = do
  chargeCaptureStep
  canonicalPath <- liftIO $ canonicalSettingsPath requestedPath
  when (depth > maxIncludeDepth)
    $ liftIO
    $ failSettings
    $ withInvalidPrefix "settings include depth exceeds the configured limit"
  when (canonicalPath `elem` ancestors)
    $ liftIO
    $ failSettings
    $ withInvalidPrefix "cyclic settings include"
  source <- captureSource canonicalPath
  traverse_
    (captureSourceGraph (depth + 1) (canonicalPath : ancestors))
    (Map.elems source.sourceIncludes)
  pure canonicalPath

chargeCaptureStep :: State.StateT CaptureState IO ()
chargeCaptureStep = do
  steps <- State.gets (.capturedSteps)
  when (steps <= 0)
    $ liftIO
    $ failSettings
    $ withInvalidPrefix "settings include graph exceeds the configured traversal limit"
  State.modify' \captured -> captured {capturedSteps = steps - 1}

captureSource :: FilePath -> State.StateT CaptureState IO CapturedSource
captureSource canonicalPath = do
  current <- State.get
  case Map.lookup canonicalPath current.capturedSources of
    Just source -> pure source
    Nothing -> do
      when (Map.size current.capturedSources >= maxSnapshotSources)
        $ liftIO
        $ failSettings
        $ withInvalidPrefix "settings include count exceeds the configured limit"
      bytes <- liftIO $ readSnapshotBytes (maxSnapshotBytes - current.capturedBytes) canonicalPath
      events <- liftIO $ parseEvents bytes
      includePaths <- liftIO $ orFailInvalid $ rawIncludePaths events
      includes <- resolveIncludePaths canonicalPath includePaths
      let source = CapturedSource {sourceEvents = events, sourceIncludes = includes}
      State.modify' \captured ->
        captured
          { capturedSources = Map.insert canonicalPath source captured.capturedSources
          , capturedBytes = captured.capturedBytes + ByteString.length bytes
          }
      pure source

{- | Resolve each distinct @!include@ spelling in one source exactly once,
against the same include-graph budget the traversal itself spends.

Two occurrences of the same spelling must name the same file: resolving them
separately would let a symlink retargeted between the two lookups produce a
target that is captured but never expanded, so the bytes this process read and
the bytes it analyzed and loaded would no longer be the same set. The
resolution recorded here is what capture, raw analysis and the load all use.

Resolving is filesystem work, so it is charged as it happens: a source naming
a hundred thousand distinct includes is refused after the budget's worth of
them rather than after all of them have been canonicalized.
-}
resolveIncludePaths
  :: FilePath -> [ByteString] -> State.StateT CaptureState IO (Map ByteString FilePath)
resolveIncludePaths canonicalPath = foldM resolveOnce mempty
 where
  resolveOnce resolved spelling
    | Map.member spelling resolved = pure resolved
    | otherwise = do
        chargeCaptureStep
        let relative = TextEncoding.decodeUtf8With lenientDecode spelling
        target <-
          liftIO $ canonicalSettingsPath (takeDirectory canonicalPath </> toString relative)
        pure $ Map.insert spelling target resolved

canonicalSettingsPath :: FilePath -> IO FilePath
canonicalSettingsPath path =
  Exception.catch
    (canonicalizePath path)
    (\(_ :: Exception.IOException) -> failSettings unreadableSource)

{- | Read one settings source through a single open handle, never reading more
than the snapshot's remaining byte budget.

The handle is the source's identity: its size and its bytes come from the same
open file description, so a path re-pointed after the open cannot change what
this process read. A source that is not a regular file — a FIFO, a device, a
socket — has no size to check and could deliver bytes forever, so it is
refused before any read rather than after one that never returns.
-}
readSnapshotBytes :: Int -> FilePath -> IO ByteString
readSnapshotBytes remainingBudget path = do
  when (remainingBudget < 0) $ failSettings byteLimitExceeded
  Exception.bracket (openSnapshotSource path) hClose \handle -> do
    size <- regularSourceSize handle
    when (size > toInteger remainingBudget) $ failSettings byteLimitExceeded
    bytes <- readSnapshotBudget handle
    when (ByteString.length bytes > remainingBudget) $ failSettings byteLimitExceeded
    pure bytes
 where
  openSnapshotSource source =
    Exception.catch
      (openBinaryFile source ReadMode)
      (\(_ :: Exception.IOException) -> failSettings unreadableSource)

  -- 'hFileSize' fails for anything but a regular file, which is exactly the
  -- distinction this needs and the only one @base@ offers.
  regularSourceSize handle =
    Exception.catch
      (hFileSize handle)
      (\(_ :: Exception.IOException) -> failSettings nonRegularSource)

  -- One byte past the budget is enough to prove a source that grew between
  -- the size check and the read is over it.
  readSnapshotBudget handle =
    Exception.catch
      (ByteString.hGet handle (remainingBudget + 1))
      (\(_ :: Exception.IOException) -> failSettings unreadableSource)

parseEvents :: ByteString -> IO [Libyaml.Event]
parseEvents bytes = do
  events <- onMalformedYaml $ runConduitRes $ Libyaml.decode bytes .| boundedSourceEvents
  orFail events

{- | Collect one source's events with the event count and the collection depth
charged as the parser emits them, so an event-dense or deeply nested source is
refused while it is being parsed rather than after it has been materialized.
Refusal stops the parser by returning, so the failure is one diagnostic rather
than an exception thrown through the parser's own cleanup.
-}
boundedSourceEvents :: Monad m => ConduitT Libyaml.Event o m (Either Text [Libyaml.Event])
boundedSourceEvents = go 0 0 id
 where
  go
    :: Monad m
    => Int
    -> Int
    -> ([Libyaml.Event] -> [Libyaml.Event])
    -> ConduitT Libyaml.Event o m (Either Text [Libyaml.Event])
  go !count !depth acc =
    await >>= \case
      Nothing -> pure $ Right (acc [])
      Just event
        | count >= maxSourceEvents -> pure $ Left eventLimitExceeded
        | depth + eventDepthChange event > maxNodeDepth -> pure $ Left depthLimitExceeded
        | otherwise -> go (count + 1) (depth + eventDepthChange event) (acc . (event :))

eventDepthChange :: Libyaml.Event -> Int
eventDepthChange = \case
  Libyaml.EventMappingStart {} -> 1
  Libyaml.EventSequenceStart {} -> 1
  Libyaml.EventMappingEnd -> -1
  Libyaml.EventSequenceEnd -> -1
  _ -> 0

{- | Turn the YAML parser's own failures into a startup diagnostic, and only
those: an unrelated or asynchronous exception is left alone rather than being
reported as invalid configuration.
-}
onMalformedYaml :: IO a -> IO a
onMalformedYaml action =
  action
    `Exception.catches` [ Exception.Handler \(_ :: Yaml.ParseException) -> failSettings invalidYaml
                        , Exception.Handler \(_ :: Libyaml.YamlException) -> failSettings invalidYaml
                        ]

rawIncludePaths :: [Libyaml.Event] -> Either Text [ByteString]
rawIncludePaths = traverse includePath . filter isInclude
 where
  isInclude = \case
    Libyaml.EventScalar _ (Libyaml.UriTag "!include") _ _ -> True
    _ -> False

  includePath = \case
    Libyaml.EventScalar bytes (Libyaml.UriTag "!include") _ _
      | ByteString.null bytes -> Left "an !include path cannot be empty"
      | otherwise -> Right bytes
    _ -> Left "malformed !include source"

{- | Splice the captured include bytes into each captured root event stream,
dropping the same stream and document events @Data.Yaml.Include@ drops.

The event budget spans the whole snapshot rather than restarting per root, so
naming more roots cannot buy more expansion than one snapshot is allowed.
-}
expandCapturedSources
  :: Map FilePath CapturedSource -> [FilePath] -> Either Text [[Libyaml.Event]]
expandCapturedSources sources roots = State.evalStateT (traverse go roots) maxExpandedEvents
 where
  go :: FilePath -> State.StateT Int (Either Text) [Libyaml.Event]
  go path = do
    source <-
      lift $ maybe (Left "a captured !include source is missing") Right $ Map.lookup path sources
    concat <$> traverse (expandEvent source) source.sourceEvents

  expandEvent source event = do
    chargeExpandedEvent
    case event of
      Libyaml.EventScalar bytes (Libyaml.UriTag "!include") _ _ -> do
        target <-
          lift
            $ maybe (Left "a captured !include target is missing") Right
            $ Map.lookup bytes source.sourceIncludes
        included <- go target
        pure $ filter (`notElem` irrelevantEvents) included
      _ -> pure [event]

  chargeExpandedEvent = do
    remaining <- State.get
    when (remaining <= 0) $ lift $ Left "settings includes expand past the configured event limit"
    State.put (remaining - 1)

  irrelevantEvents =
    [ Libyaml.EventStreamStart
    , Libyaml.EventDocumentStart
    , Libyaml.EventDocumentEnd
    , Libyaml.EventStreamEnd
    ]

decodeExpandedValue :: [Libyaml.Event] -> IO Value
decodeExpandedValue events =
  onMalformedYaml
    $ YamlInternal.decodeHelper_ (Conduit.sourceList events) >>= \case
      Left _ -> failSettings invalidYaml
      Right (_warnings, value) -> pure value

-- | The embedded bytes are analyzed with every other captured source, against
-- the one snapshot-wide budget, so this only decodes.
decodeEmbeddedBytes :: ByteString -> IO Value
decodeEmbeddedBytes bytes =
  case Yaml.decodeEither' bytes of
    Left _ -> failSettings invalidYaml
    Right value -> pure value

{- | Check decoded settings values, after YAML has resolved anchors, aliases
and merge keys but before any environment substitution. Runtime sources are
additionally analyzed as raw events, so duplicate and aliased locale keys stay
visible there; this pass covers values a caller supplies directly.
-}
validateLocaleCatalogSettingsValues :: [Value] -> Either Text ()
validateLocaleCatalogSettingsValues = traverse_ validateValue

validateValue :: Value -> Either Text ()
validateValue = \case
  Object object ->
    traverse_
      ( \(key, value) -> do
          for_ (settingForKey $ Key.toText key) (`validateEnvironmentMarker` value)
          validateValue value
      )
      (KeyMap.toList object)
  Array values -> traverse_ validateValue values
  _ -> Right ()

{- | Check the raw environment string behind every canonical marker that
survives the effective merge. A marker a higher-precedence source replaced
cannot reach its variable, so its variable is not this deployment's to
validate — exactly as the standard loader would ignore it.
-}
validateCanonicalEnvironmentValues :: KeyMap Text -> Value -> Either Text ()
validateCanonicalEnvironmentValues environment = go
 where
  go = \case
    Object object ->
      traverse_
        ( \(key, value) -> do
            for_ (settingForKey $ Key.toText key) \setting ->
              for_ (canonicalEnvironmentMarkers setting value) \name ->
                for_ (KeyMap.lookup (Key.fromText name) environment) \raw ->
                  validateLocaleCatalogRawEnvironmentValue setting raw
            go value
        )
        (KeyMap.toList object)
    Array values -> traverse_ go values
    _ -> Right ()

{- | The variables an @_env:@ marker under a locale setting would actually
read, when that is the setting's own canonical variable. @applyEnvValue@
substitutes inside nested objects and arrays too, so those are collected as
well; a noncanonical name is refused by the raw analysis instead.
-}
canonicalEnvironmentMarkers :: LocaleCatalogSetting -> Value -> [Text]
canonicalEnvironmentMarkers setting = \case
  String marker -> maybeToList $ canonicalEnvironmentMarker setting marker
  Array values -> concatMap (canonicalEnvironmentMarkers setting) (toList values)
  Object fields -> concatMap (canonicalEnvironmentMarkers setting) (KeyMap.elems fields)
  _ -> []

{- | @applyEnvValue@ reads the variable whether or not a default follows, so
both spellings count here.
-}
canonicalEnvironmentMarker :: LocaleCatalogSetting -> Text -> Maybe Text
canonicalEnvironmentMarker setting marker = do
  suffix <- Text.stripPrefix "_env:" marker
  let name = Text.takeWhile (/= ':') suffix
  guard (name == localeCatalogSettingEnvVar setting)
  pure name

{- | Analyze one captured, include-expanded event stream: resolve anchors and
aliases the way @yaml@ does, then check every mapping — including the ones a
@\<\<@ merge key pulls in — before that merge is resolved.
-}
analyzeCapturedEvents :: [[Libyaml.Event]] -> Either Text ()
analyzeCapturedEvents = void . foldM analyzeStream maxAnalysisSteps
 where
  analyzeStream budget events = do
    (documents, remaining) <- parseRawDocuments budget events
    State.execStateT (traverse_ analyzeRawNode documents) remaining

{- | Turn one include-expanded event stream into nodes, charging the shared
analysis budget per node and refusing nesting past the configured depth. The
budget left over is what the analysis itself may spend, so materialization and
analysis together stay inside one advertised bound.
-}
parseRawDocuments :: Int -> [Libyaml.Event] -> Either Text ([RawNode], Int)
parseRawDocuments budget events = do
  (documents, parsed) <-
    State.runStateT
      (go $ filter structural events)
      RawParseState {rawAnchors = mempty, rawBudget = budget}
  pure (documents, parsed.rawBudget)
 where
  structural = \case
    Libyaml.EventStreamStart -> False
    Libyaml.EventStreamEnd -> False
    Libyaml.EventDocumentStart -> False
    Libyaml.EventDocumentEnd -> False
    _ -> True

  go [] = pure []
  go remaining = do
    (node, rest) <- parseRawNode 0 remaining
    (node :) <$> go rest

parseRawNode :: Int -> [Libyaml.Event] -> RawParse (RawNode, [Libyaml.Event])
parseRawNode depth = \case
  Libyaml.EventAlias anchor : rest -> do
    chargeRawStep
    anchors <- State.gets (.rawAnchors)
    case Map.lookup anchor anchors of
      Nothing -> lift $ Left "settings YAML references an anchor that is not defined yet"
      Just node -> pure (node, rest)
  Libyaml.EventScalar bytes _ _ anchor : rest -> do
    chargeRawStep
    (,rest) <$> defineRawAnchor anchor (RawScalar bytes)
  Libyaml.EventSequenceStart _ _ anchor : rest -> do
    chargeRawStep
    nested <- descendRawNode depth
    (values, remaining) <- parseRawSequence nested rest
    (,remaining) <$> defineRawAnchor anchor (RawSequence values)
  Libyaml.EventMappingStart _ _ anchor : rest -> do
    chargeRawStep
    nested <- descendRawNode depth
    (pairs, remaining) <- parseRawMapping nested rest
    (,remaining) <$> defineRawAnchor anchor (RawMapping pairs)
  _ -> lift $ Left "malformed settings YAML"

chargeRawStep :: RawParse ()
chargeRawStep = do
  budget <- State.gets (.rawBudget)
  when (budget <= 0) $ lift $ Left analysisLimitExceeded
  State.modify' \parsed -> parsed {rawBudget = budget - 1}

{- | An include-expanded stream can nest deeper than any single source did, so
depth is charged here as well as while each source was parsed.
-}
descendRawNode :: Int -> RawParse Int
descendRawNode depth
  | depth >= maxNodeDepth = lift $ Left depthLimitExceeded
  | otherwise = pure (depth + 1)

-- | An anchor names a node only once that node is complete, and a repeated
-- anchor name replaces the earlier one, which is what @yaml@ does.
defineRawAnchor :: Libyaml.Anchor -> RawNode -> RawParse RawNode
defineRawAnchor anchor node = do
  for_ anchor \name ->
    State.modify' \parsed -> parsed {rawAnchors = Map.insert name node parsed.rawAnchors}
  pure node

parseRawSequence :: Int -> [Libyaml.Event] -> RawParse ([RawNode], [Libyaml.Event])
parseRawSequence depth = go []
 where
  go values = \case
    Libyaml.EventSequenceEnd : rest -> pure (reverse values, rest)
    events -> do
      (value, remaining) <- parseRawNode depth events
      go (value : values) remaining

parseRawMapping :: Int -> [Libyaml.Event] -> RawParse ([(RawNode, RawNode)], [Libyaml.Event])
parseRawMapping depth = go []
 where
  go pairs = \case
    Libyaml.EventMappingEnd : rest -> pure (reverse pairs, rest)
    events -> do
      (key, afterKey) <- parseRawNode depth events
      (value, remaining) <- parseRawNode depth afterKey
      go ((key, value) : pairs) remaining

analyzeRawNode :: RawNode -> Analysis ()
analyzeRawNode node = do
  chargeAnalysisStep
  case node of
    RawScalar {} -> pure ()
    RawSequence values -> traverse_ analyzeRawNode values
    RawMapping pairs -> do
      void $ effectiveLocaleKeys node
      traverse_ (\(key, value) -> analyzeRawNode key *> analyzeRawNode value) pairs

chargeAnalysisStep :: Analysis ()
chargeAnalysisStep = do
  remaining <- State.get
  when (remaining <= 0) $ lift $ Left analysisLimitExceeded
  State.put (remaining - 1)

{- | The locale settings one mapping represents, counting the keys a @\<\<@
merge contributes. Two representations of the same setting are ambiguous and
are refused, whether they are spelled directly, reached through a scalar
alias, or contributed by different merge sources.
-}
effectiveLocaleKeys :: RawNode -> Analysis (Set LocaleCatalogSetting)
effectiveLocaleKeys = \case
  RawMapping pairs -> foldM addPair mempty pairs
  _ -> pure mempty
 where
  addPair keys (key, value) = do
    chargeAnalysisStep
    let textKey = rawTextKey key
        direct = textKey >>= settingForKey
    traverse_ (`analyzeLocaleValue` value) direct
    merged <- if textKey == Just mergeKey then mergedLocaleKeys value else pure mempty
    lift $ addLocaleKeys keys (Set.fromList (toList direct) <> merged)

{- | What a @\<\<@ merge value contributes, exactly as
@Data.Yaml.Internal@'s @mergeObjects@ decides it: a mapping merges its own
keys, a sequence merges only its immediate mapping elements, and everything
else — a nested sequence, a scalar — contributes nothing at all. Recursing
into a nested sequence here would refuse configuration the loader accepts.
-}
mergedLocaleKeys :: RawNode -> Analysis (Set LocaleCatalogSetting)
mergedLocaleKeys node = do
  chargeAnalysisStep
  case node of
    RawMapping {} -> effectiveLocaleKeys node
    RawSequence values -> foldM addMergedElement mempty values
    RawScalar {} -> pure mempty
 where
  addMergedElement keys = \case
    element@RawMapping {} -> do
      chargeAnalysisStep
      contributed <- effectiveLocaleKeys element
      lift $ addLocaleKeys keys contributed
    _ -> pure keys

addLocaleKeys
  :: Set LocaleCatalogSetting -> Set LocaleCatalogSetting -> Either Text (Set LocaleCatalogSetting)
addLocaleKeys existing incoming =
  case Set.lookupMin $ Set.intersection existing incoming of
    Nothing -> Right $ existing <> incoming
    Just setting -> reject setting "is represented more than once in one YAML mapping or merge"

mergeKey :: Text
mergeKey = "<<"

{- | @yaml@ builds mapping keys from the scalar's raw text, and decodes YAML
bytes leniently, so this reads a key exactly the way the loader will.
-}
rawTextKey :: RawNode -> Maybe Text
rawTextKey = \case
  RawScalar bytes -> Just $ TextEncoding.decodeUtf8With lenientDecode bytes
  _ -> Nothing

settingForKey :: Text -> Maybe LocaleCatalogSetting
settingForKey key =
  find (\setting -> localeCatalogSettingKey setting == key) [minBound .. maxBound]

{- | Every raw value a locale setting's key carries, before YAML resolves the
merge that key participates in. @applyEnvValue@ substitutes nested values too,
so a marker inside a sequence or a nested mapping is checked with the same
grammar as the plain scalar spelling.
-}
analyzeLocaleValue :: LocaleCatalogSetting -> RawNode -> Analysis ()
analyzeLocaleValue setting node = do
  chargeAnalysisStep
  case node of
    RawScalar bytes ->
      lift $ validateMarkerText setting $ TextEncoding.decodeUtf8With lenientDecode bytes
    RawSequence values -> traverse_ (analyzeLocaleValue setting) values
    RawMapping pairs -> traverse_ (analyzeLocaleValue setting . snd) pairs

validateEnvironmentMarker :: LocaleCatalogSetting -> Value -> Either Text ()
validateEnvironmentMarker setting = \case
  String marker -> validateMarkerText setting marker
  Array values -> traverse_ (validateEnvironmentMarker setting) values
  Object fields -> traverse_ (validateEnvironmentMarker setting) (KeyMap.elems fields)
  _ -> Right ()

validateMarkerText :: LocaleCatalogSetting -> Text -> Either Text ()
validateMarkerText setting marker
  | Just suffix <- Text.stripPrefix "_env:" marker =
      case Text.break (== ':') suffix of
        (name, remainder)
          | Text.null remainder -> reject setting "is an incomplete _env: mapping"
          | name == localeCatalogSettingEnvVar setting -> Right ()
          | otherwise ->
              reject setting "must use only its canonical ARKHAM_LOCALE_CATALOG_* environment variable"
  | otherwise = Right ()

reject :: LocaleCatalogSetting -> Text -> Either Text a
reject setting reason =
  Left
    $ localeCatalogSettingKey setting
    <> " ("
    <> localeCatalogSettingEnvVar setting
    <> ") "
    <> reason
