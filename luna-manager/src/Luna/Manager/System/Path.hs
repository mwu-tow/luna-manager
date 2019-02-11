module Luna.Manager.System.Path where

import Prologue hiding (FilePath, fromText, null)

import Luna.Manager.Logger     (LoggerMonad)
import Luna.Manager.System.Env

import           Filesystem.Path           (FilePath, null)
import qualified Filesystem.Path           as Path
import           Filesystem.Path.CurrentOS (encodeString, fromText)

type URIPath  = Text

instance Convertible Text FilePath where
    convert = fromText

expand :: (LoggerMonad m, MonadIO m) => FilePath -> m FilePath
expand path = if null path
    then return path
    else do
        let dirs  = Path.splitDirectories path
            fstEl = unsafeHead dirs -- FIXME
        home <- getHomePath
        current <- getCurrentPath
        case encodeString fstEl of
            "~/"   -> return $ Path.concat $ home : unsafeTail dirs -- FIXME
            "./"   -> return $ Path.concat $ current : unsafeTail dirs -- FIXME
            "../"  -> return $ Path.concat $ Path.parent current : unsafeTail dirs -- FIXME
            "~\\"  -> return $ Path.concat $ home : unsafeTail dirs -- FIXME
            ".\\"  -> return $ Path.concat $ current : unsafeTail dirs -- FIXME
            "..\\" -> return $ Path.concat $ Path.parent current : unsafeTail dirs -- FIXME
            _      -> return path
