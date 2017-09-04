module System.FileLock.Extended
    ( module System.FileLock
    , simpleFileLock
    ) where

import           Control.Exception (bracket)
import           System.Directory  (removeFile)
import           System.FileLock
import           System.FilePath   ((<.>))

simpleFileLock
    :: FilePath   -- ^ The file you want to lock.  ".lock" will be appended.
    -> IO a       -- ^ The IO action you wish to execute with the locked file.
    -> IO a       -- ^ Result
simpleFileLock fp0 action = bracket
    (lockFile fp1 Exclusive)
    (\lock -> removeFile fp1 >> unlockFile lock)
    (\_lock -> action)
  where
    fp1 = fp0 <.> "lock"

