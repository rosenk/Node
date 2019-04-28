{-# LANGUAGE TemplateHaskell #-}
module Enecuum.Core.FileSystem.Language where

import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.Lazy.Internal   as BSI
import           Enecuum.Prelude
import           Language.Haskell.TH.MakeFunctor


class Monad m => FileSystem m where
    readFile :: FilePath -> m B.ByteString
    writeFile :: FilePath -> B.ByteString -> m ()
    appendFile :: FilePath -> B.ByteString -> m ()
    getHomeDirectory :: m FilePath
    createFilePath :: FilePath -> m FilePath
    doesFileExist :: FilePath -> m Bool
