module Enecuum.Core.FileSystem.Interpreter where

import qualified Data.ByteString.Lazy  as B
import qualified Enecuum.Core.Language as L
import           Enecuum.Prelude
import           System.Directory      (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath.Posix (splitFileName)


instance L.FileSystem IO where
    readFile   = B.readFile
    writeFile  = B.writeFile
    appendFile = B.appendFile
    getHomeDirectory = getHomeDirectory
    createFilePath filepath = do
        let (dir, filename) = splitFileName filepath
        createDirectoryIfMissing True dir
        pure filepath
    doesFileExist = doesFileExist
