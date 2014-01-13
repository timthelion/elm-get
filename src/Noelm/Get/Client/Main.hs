{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where
 
{- external libraries -}
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import System.IO
import Control.Monad.Error
import qualified Paths_noelm_get as This
import qualified Data.Maybe as Maybe
import Data.Version (showVersion)

{- modules from base Noelm package -}
import qualified Noelm.Internal.Name   as N

{- internal modules -}
import qualified Noelm.Get.Client.Install  as Install
import qualified Noelm.Get.Client.Publish  as Publish
import qualified Noelm.Get.Client.Registry as Registry
import qualified Noelm.Get.Utils.Commands  as Cmd

data Commands
    = Install { lib :: String, version :: Maybe String }
    | Update { libs :: [String] }
    | Publish
      deriving (Data, Typeable, Show, Eq)

commands =
    [ Install { lib = def &= argPos 0 &= typ "LIBRARY"
              , version = Nothing &= args &= typ "VERSION"
              }
      &= help "Install libraries in the local project."
      &= details [ "Examples:"
                 , "  noelm-get install            # install everything needed by build.json"
                 , "  noelm-get install tom/Array  # install a specific github repo" ]
    , Update { libs = [] &= args &= typ "LIBRARY" }
      &= help "Check for updates to any local libraries, ask to upgrade."
      &= details [ "Examples:"
                 , "  noelm-get update             # check for updates to local libraries"
                 , "  noelm-get update tom/Array   # update from a specific github repo" ]
    , Publish
      &= help "Publish project to the central repository."
      &= details []
    ]

myModes :: Mode (CmdArgs Commands)
myModes = cmdArgsMode $ modes commands
    &= versionArg [explicit, name "version", name "v", summary info]
    &= summary (info ++ ", (c) 2013 Evan Czaplicki & 2014 Timothy Hobbs")
    &= help "install, update, and publish elm libraries"
    &= helpArg [explicit, name "help", name "h"]
    &= program "noelm-get"
 
info = "noelm-get " ++ showVersion This.version

main :: IO ()
main = do
  args <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
  result <- runErrorT (handle opts)
  case result of
    Right _ -> return ()
    Left err ->
        do hPutStr stderr ("\nError: " ++ err ++ newline)
           exitFailure
        where
          newline = if last err == '\n' then "" else "\n"

handle :: Commands -> ErrorT String IO ()
handle options =
    case options of
      Install { lib=library, version=maybeVersion } ->
          do name <- N.fromString' library
             Install.install name maybeVersion

      Publish -> Publish.publish

      _ -> do Cmd.out "Not implemented yet!"
              liftIO $ print options