module Data.Foscam.Sort(
  foscamsort
) where

import Control.Category(id, (.))
import Control.Lens((^.), (^?), _2)
import Control.Monad((>>=), when, unless)
import Data.Bool(Bool(True))
import Data.Either(either)
import Data.Foldable(Foldable, mapM_)
import Data.Foscam.Directory(getFoscamDirectoryContents, FoscamDirectoryFile, _FoscamDirectory, _FoscamFilename, _Is)
import Data.Foscam.File(getFilename)
import Data.Foscam.Sort.FilenamePath(FilenamePath, yyyy, mm, dd, nn, (/<>/), (~/>))
import Data.Maybe(maybe)
import Data.Monoid((<>))
import System.Directory(doesFileExist, getPermissions, readable, renameFile, doesFileExist, createDirectoryIfMissing)
import System.Environment(getArgs, getProgName)
import System.FilePath(FilePath, (</>))
import System.IO(IO, hPutStrLn, stderr)
import System.Posix.Files(createSymbolicLink)

foscamsort ::
  IO ()
foscamsort = 
  do a <- getArgs
     case a of
       (i:o:_) ->
         getFoscamDirectoryContents i >>=
         mapM_ (move o (yyyy <> mm <> dd) [yyyy, yyyy /<>/ mm, yyyy /<>/ mm /<>/ dd, yyyy /<>/ mm /<>/ dd /<>/ nn]) 
       _ ->
         do p <- getProgName
            hPutStrLn stderr ("Usage: " <> p <> " <foscam-files-directory> <sort-output-directory>")

move ::
  Foldable t =>
  FilePath
  -> FilenamePath
  -> t FilenamePath
  -> FoscamDirectoryFile
  -> IO ()
move base mvpath lnpaths q =
  let fname = q ^? _Is . _2
      name = either id getFilename (q ^. _FoscamFilename)
      mv = base </> "move" </> maybe "unknown" (\n -> "date" </> mvpath ~/> n) fname
      mvto = mv </> name
  in do createDirectoryIfMissing True mv
        moveFile (q ^. _FoscamDirectory </> name) mvto
        mapM_ (\n -> mapM_
                (\k -> 
                  let out = base </> "link" </> k ~/> n
                  in do createDirectoryIfMissing True out
                        createSymbolicLinkIfMissing mvto (out </> name)) lnpaths) fname
     
moveFile ::
  FilePath
  -> FilePath
  -> IO ()
moveFile f t =
  do e <- doesFileExist f
     when e (do p <- getPermissions f
                when (readable p) (renameFile f t))

createSymbolicLinkIfMissing ::
  FilePath
  -> FilePath
  -> IO ()
createSymbolicLinkIfMissing file1 file2 =
        do s <- doesFileExist file2
           unless s (createSymbolicLink file1 file2)
