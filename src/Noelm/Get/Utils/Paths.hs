module Noelm.Get.Utils.Paths where

import System.FilePath
import qualified Noelm.Internal.Name as N
import qualified Noelm.Internal.Version as V

internals = "_internals"

libDir = "public" </> "catalog"

json = "docs.json"
index = "index.noelm"
listing = "public" </> "libraries.json"
listingBits = "listing.bits"

library name = libDir </> N.toFilePath name

libraryVersion :: N.Name -> V.Version -> FilePath
libraryVersion name version = library name </> show version

moduleToNoelmFile :: String -> FilePath
moduleToNoelmFile moduleName = swapDots moduleName ++ ".noelm"

moduleToJsonFile :: String -> FilePath
moduleToJsonFile moduleName = "docs" </> swapDots moduleName ++ ".json"

swapDots :: String -> String
swapDots = map (\c -> if c == '.' then '/' else c)

combinedJson :: FilePath
combinedJson = "docs" </> "docs.json"