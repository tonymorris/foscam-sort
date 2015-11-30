module Data.Foscam.Sort where

import Data.Monoid
import Control.Monad
import Data.Foscam.File -- (Filename(..), AsDate(..), AsTime(..))
import Data.Foscam.Sort.FilenamePath
import Data.Foscam.Directory
import Prelude
import Control.Lens
import System.Directory
import System.FilePath
import System.Posix.Files

main ::
  IO ()
main = 
  let p = [yyyy <> mm <> dd, yyyy /<>/ mm, yyyy /<>/ mm /<>/ dd] -- , yyyymm, yyyymmdd, yyyymmddhh, yyyymmddhhmm]
      q = join [(deviceIdpath /<>/) <$> p, (aliaspath /<>/) <$> p, p]
  in run q "/home/tmorris/Desktop/test/files" "/home/tmorris/Desktop/test/out"
  
pr = 
  getFoscamDirectoryContents "/home/tmorris/Desktop/test/files" >>=
  mapM_ move

move q =
  let (name, w) = case q of
                    Isn't _ n -> (n, "unknown")
                    Is    _ n -> (getFilename n, "date" </> (yyyy <> mm <> dd) ~/> n)
      d = "/home/tmorris/Desktop/test/move/" </> w
      a1 = q ^. _FoscamDirectory </> name
      a2 = d </> name
  in do createDirectoryIfMissing True d
        putStrLn ("Moving " ++ a1 ++ " to " ++ a2)
        copyFileIf a1 a2 -- todo move
        createDirectoryIfMissing True "/home/tmorris/Desktop/test/link/"
        createSymbolicLinkIfMissing
          "../move/date/20150205/00626E44C831(house)_1_20150205220252_23195.jpg"
          "/home/tmorris/Desktop/test/link/xyz"
 
copyFileIf ::
  FilePath
  -> FilePath
  -> IO ()
copyFileIf fr to =
  do e <- doesFileExist fr
     when e (do p <- getPermissions fr
                when (readable p) (copyFile fr to))

createSymbolicLinkIfMissing ::
  FilePath
  -> FilePath
  -> IO ()
createSymbolicLinkIfMissing file1 file2 =
        do s <- doesFileExist file2
           print (s, file2)
           unless s (createSymbolicLink file1 file2)

run ::
  [FilenamePath]
  -> FilePath
  -> FilePath
  -> IO ()
run p d e =
  undefined {-}
  do c <- getFoscamDirectoryContents d
     mapM_ (\w -> links w p d e) (filenames c)
-}
-- move :: 
--   FilePath [directory containing file]
--   Filename [filename in directory]
--   (FilePath, FilenamePath) [move to]
--   (FilePath, [FilenamePath]) [link to]
links ::
  Filename
  -> [FilenamePath]
  -> FilePath
  -> FilePath
  -> IO ()
links c q d e =
  let name = getFilename c
      createSymbolicLinkIfMissing file1 file2 =
        do s <- doesFileExist file2
           unless s (createSymbolicLink file1 file2)
  in mapM_ (\p -> let outdir = e </> p ~/> c
                  in do createDirectoryIfMissing True outdir
                        createSymbolicLinkIfMissing (d </> name) (outdir </> name)) q

