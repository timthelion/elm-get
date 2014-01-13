{-# LANGUAGE OverloadedStrings #-}
module Noelm.Get.Client.Publish where

{- external libraries -}
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Error
import System.Directory
import System.Exit
import System.FilePath (replaceExtension, (</>))
import System.IO
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Text.JSON
import Data.Version

{- modules from base Noelm package -}
import qualified Noelm.Internal.Dependencies as D
import qualified Noelm.Internal.Paths        as NPath
import qualified Noelm.Internal.Name         as N
import qualified Noelm.Internal.Version      as V

{- internal modules -}
import qualified Noelm.Get.Client.Registry  as R
import qualified Noelm.Get.Utils.Paths      as Path
import qualified Noelm.Get.Utils.Commands   as Cmd
import qualified Noelm.Get.Utils.Http       as Http
import qualified Noelm.Get.Utils.PrettyJson as Pretty

publish :: ErrorT String IO ()
publish =
  do deps <- getDeps
     let name = D.name deps
         version = D.version deps
         exposedModules = D.exposed deps
     Cmd.out $ unwords [ "Verifying", show name, show version, "..." ]
     verifyNoDependencies (D.dependencies deps)
     verifyNoelmVersion (D.noelmVersion deps)
     verifyExposedModules exposedModules
     verifyVersion name version
     withCleanup $ do
       generateDocs exposedModules
       R.register name version Path.combinedJson
     Cmd.out "Success!"

getDeps :: ErrorT String IO D.Deps
getDeps =
  do either <- liftIO $ runErrorT $ D.depsAt NPath.dependencyFile
     case either of
       Right deps -> return deps
       Left err ->
           liftIO $ do
             hPutStrLn stderr $ "\nError: " ++ err
             hPutStr stdout $ "\nWould you like me to add the missing fields? (y/n) "
             yes <- Cmd.yesOrNo
             case yes of
               False -> hPutStrLn stdout "Okay, maybe next time!"
               True -> do
                 addMissing =<< readFields
                 hPutStrLn stdout $ "Done! Now go through " ++ NPath.dependencyFile ++
                      " and check that\neach field is filled in with valid and helpful information."
             exitFailure
            
addMissing :: Map.Map String JSValue -> IO ()
addMissing existingFields =
    writeFile NPath.dependencyFile $ show $ Pretty.object obj'
    where
      obj' = map (\(f,v) -> (f, Maybe.fromMaybe v (Map.lookup f existingFields))) obj

      str = JSString . toJSString
      obj = [ ("version", str "0.1")
            , ("summary", str "concise, helpful summary of your project")
            , ("description", str "full description of this project, describe your use case")
            , ("license", str "LGPLv3")
            , ("repository", str "https://github.com/USER/PROJECT.git")
            , ("exposed-modules", JSArray [])
            , ("noelm-version", str $ show V.noelmVersion)
            , ("dependencies", JSObject $ toJSObject [])
            ]

readFields :: IO (Map.Map String JSValue)
readFields =
    do exists <- doesFileExist NPath.dependencyFile
       case exists of
         False -> return Map.empty
         True -> do raw <- readFile NPath.dependencyFile
                    case decode raw of
                      Error err -> return Map.empty
                      Ok obj -> return (Map.fromList $ fromJSObject obj)

withCleanup :: ErrorT String IO () -> ErrorT String IO ()
withCleanup action =
    do existed <- liftIO $ doesDirectoryExist "docs"
       either <- liftIO $ runErrorT action
       when (not existed) $ liftIO $ removeDirectoryRecursive "docs"
       case either of
         Left err -> throwError err
         Right () -> return ()

verifyNoDependencies :: [(N.Name,V.Version)] -> ErrorT String IO ()
verifyNoDependencies [] = return ()
verifyNoDependencies _ =
    throwError
        "noelm-get is not able to publish projects with dependencies."

verifyNoelmVersion :: V.Version -> ErrorT String IO ()
verifyNoelmVersion noelmVersion@(V.V ns _)
    | ns == ns' = return ()
    | otherwise =
        throwError $ "noelm_dependencies.json says this project depends on version " ++
                     show noelmVersion ++ " of the compiler but the compiler you " ++
                     "have installed is version " ++ show V.noelmVersion
    where
      V.V ns' _ = V.noelmVersion

verifyExposedModules :: [String] -> ErrorT String IO ()
verifyExposedModules modules =
    do when (null modules) $ throwError $
              "There are no exposed modules in " ++ NPath.dependencyFile ++
              "!\nAll libraries must make at least one module available to users."
       mapM_ verifyExists modules
    where
      verifyExists modul =
          let path = Path.moduleToNoelmFile modul in
          do exists <- liftIO $ doesFileExist path
             when (not exists) $ throwError $
                 "Cannot find module " ++ modul ++ " at " ++ path

verifyVersion :: N.Name -> V.Version -> ErrorT String IO ()
verifyVersion name version =
    do response <- R.versions name
       case response of
         Nothing -> return ()
         Just versions ->
             do let maxVersion = maximum (version:versions)
                when (version < maxVersion) $ throwError $ unlines
                     [ "a later version has already been released."
                     , "Use a version number higher than " ++ show maxVersion ]
                checkSemanticVersioning maxVersion

       checkTag version

    where
      checkSemanticVersioning _ = return ()

      checkTag version = do
        tags <- lines <$> Cmd.git [ "tag", "--list" ]
        let v = show version
        when (show version `notElem` tags) $
             throwError (unlines (tagMessage v))

      tagMessage v =
          [ "Libraries must be tagged in git, but tag " ++ v ++ " was not found."
          , "These tags make it possible to find this specific version on github."
          , "To tag the most recent commit and push it to github, run this:"
          , ""
          , "    git tag -a " ++ v ++ " -m \"release version " ++ v ++ "\""
          , "    git push origin " ++ v
          , ""
          ]

generateDocs :: [String] -> ErrorT String IO ()
generateDocs modules = 
    do forM noelms $ \path -> Cmd.run "noelm-doc" [path]
       liftIO $ do
         let path = Path.combinedJson
         BS.writeFile path "[\n"
         let addCommas = List.intersperse (BS.appendFile path ",\n")
         sequence_ $ addCommas $ map append jsons
         BS.appendFile path "\n]"

    where
      noelms = map Path.moduleToNoelmFile modules
      jsons = map Path.moduleToJsonFile modules

      append :: FilePath -> IO ()
      append path = do
        json <- BS.readFile path
        BS.length json `seq` return ()
        BS.appendFile Path.combinedJson json
