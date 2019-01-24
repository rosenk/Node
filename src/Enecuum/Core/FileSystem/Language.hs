{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.FileSystem.Language where

import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as BSI


class FileSystem m where
    readFile :: FilePath -> m B.ByteString
    writeFile :: FilePath -> B.ByteString -> m ()
    appendFile :: FilePath -> B.ByteString -> m ()
    getHomeDirectory :: m FilePath
    createFilePath :: FilePath -> m FilePath
    doesFileExist :: FilePath -> m Bool
