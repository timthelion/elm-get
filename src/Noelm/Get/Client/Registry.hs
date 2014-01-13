{-# LANGUAGE OverloadedStrings #-}
module Noelm.Get.Client.Registry where

import Network.HTTP
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary

import Data.Version (showVersion)
import qualified Noelm.Internal.Dependencies as D
import qualified Noelm.Internal.Name         as N
import qualified Noelm.Internal.Version      as V
import qualified Noelm.Internal.Paths        as Path

import qualified Paths_noelm_get           as This
import qualified Noelm.Get.Utils.Http      as Http

domain = "http://localhost:8000"

libraryUrl path vars =
    domain ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)
  where
    version = ("noelm-get-version", showVersion This.version)

metadata :: N.Name -> ErrorT String IO (Maybe D.Deps)
metadata name =
    Http.send domain $ \manager ->
    do request  <- parseUrl $ libraryUrl "metadata" [("library", show name)]
       response <- httpLbs request manager
       return $ Json.decode $ responseBody response

versions :: N.Name -> ErrorT String IO (Maybe [V.Version])
versions name =
    Http.send domain $ \manager ->
    do request  <- parseUrl $ libraryUrl "versions" [("library", show name)]
       response <- httpLbs request manager
       return $ Binary.decode $ responseBody response

register :: N.Name -> V.Version -> FilePath -> ErrorT String IO ()
register name version path =
    Http.send domain $ \manager ->
    do request <- parseUrl $ libraryUrl "register" vars
       request' <- formDataBody files request
       let request'' = request' { responseTimeout = Nothing }
       httpLbs request'' manager
       return ()
    where
      vars = [ ("library", show name), ("version", show version) ]
      files = [ partFileSource "docs" path
              , partFileSource "deps" Path.dependencyFile
              ]
