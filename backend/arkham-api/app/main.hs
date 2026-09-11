import Application (appMain)
import Arkham.Game ()
import Arkham.Replay.ServerBuildIdentity (serverBuildIdentity)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Prelude (IO)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--build-identity"] -> BL8.putStrLn (encode serverBuildIdentity)
    _ -> appMain
