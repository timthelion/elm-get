module Noelm.Get.Utils.Commands where
         
import Control.Monad (forM_)
import Control.Monad.Error
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

yesOrNo :: IO Bool
yesOrNo = do
  hFlush stdout
  input <- getLine
  case input of
    "y" -> return True
    "n" -> return False
    _   -> do putStr "Must type 'y' for yes or 'n' for no: "
              yesOrNo

inDir :: FilePath -> ErrorT String IO a -> ErrorT String IO a
inDir dir doStuff = do
  here <- liftIO $ getCurrentDirectory
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ setCurrentDirectory dir
  result <- doStuff
  liftIO $ setCurrentDirectory here
  return result

copyDir ::  FilePath -> FilePath -> ErrorT String IO ()
copyDir src dst = do
  exists <- liftIO $ doesDirectoryExist src
  if exists
    then liftIO $ copyDir' src dst
    else throwError $ "Directory " ++ src ++ " does not exist"

copyDir' ::  FilePath -> FilePath -> IO ()
copyDir' src dst = do
  createDirectoryIfMissing True dst
  content <- getDirectoryContents src
  let paths = filter (`notElem` [".", "..",".git",".gitignore"]) content
  forM_ paths $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    (if isDirectory then copyDir' else copyFile) srcPath dstPath

git :: [String] -> ErrorT String IO String
git = run "git"

run :: String -> [String] -> ErrorT String IO String
run command args =
  do result <- liftIO runCommand
     case result of
       Right out -> return out
       Left err -> throwError $
                   "failure when running:" ++ concatMap (' ':) (command:args) ++ "\n" ++ err
  where
   runCommand = do
    (exitCode, out, err) <- readProcessWithExitCode command args ""
    let output = unlines ["Output:",out,"Errors:",err]
    return
     $ case exitCode of
      ExitSuccess   -> Right output
      ExitFailure _ -> Left  output
   

out :: String -> ErrorT String IO ()
out string = liftIO $ hPutStrLn stdout string'
    where
      string' = if not (null string) && last string == '\n' then init string else string

