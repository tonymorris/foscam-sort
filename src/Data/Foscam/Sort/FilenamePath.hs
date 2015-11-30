module Data.Foscam.Sort.FilenamePath(
  FilenamePath(FilenamePath, (~/>))
, (/<>/)
, deviceIdpath
, aliaspath
, yyyy
, mm
, dd
, hh
, nn
, ss
) where

import Data.Monoid
import Data.Digit(digitC)
import Data.Foscam.File(Filename(Filename), Date(Date), Time(Time), AsAlias(_Alias), AsDeviceId(_DeviceId))
import Data.Functor((<$>))
import Data.List((++))
import Control.Lens(( # ))
import System.FilePath(FilePath, (</>))

-- move to own module
newtype FilenamePath =
  FilenamePath { 
    (~/>) ::
      Filename
      -> FilePath
  }

instance Monoid FilenamePath where
  mempty =
    FilenamePath (\_ -> "")
  FilenamePath x `mappend` FilenamePath y =
    FilenamePath (\n -> x n ++ y n)

(/<>/) ::
  FilenamePath
  -> FilenamePath
  -> FilenamePath
FilenamePath a /<>/ FilenamePath b =
  FilenamePath (\n -> a n </> b n)

deviceIdpath ::
  FilenamePath
deviceIdpath =
  FilenamePath (\(Filename i _ _ _ _ _) -> _DeviceId # i)

aliaspath ::
  FilenamePath
aliaspath =
  FilenamePath (\(Filename _ a _ _ _ _) -> _Alias # a)

yyyy ::
  FilenamePath
yyyy =
  FilenamePath (\(Filename _ _ _ (Date y1 y2 y3 y4 _ _ _ _) _ _) -> (digitC #) <$> [y1, y2, y3, y4])

mm ::
  FilenamePath
mm =
  FilenamePath (\(Filename _ _ _ (Date _ _ _ _ m1 m2 _ _) _ _) -> (digitC #) <$> [m1, m2])

dd ::
  FilenamePath
dd =
  FilenamePath (\(Filename _ _ _ (Date _ _ _ _ _ _ d1 d2) _ _) -> (digitC #) <$> [d1, d2])

hh ::
  FilenamePath
hh =
  FilenamePath (\(Filename _ _ _ _ (Time h1 h2 _ _ _ _) _) -> (digitC #) <$> [h1, h2])

nn ::
  FilenamePath
nn =
  FilenamePath (\(Filename _ _ _ _ (Time _ _ n1 n2 _ _) _) -> (digitC #) <$> [n1, n2])

ss ::
  FilenamePath
ss =
  FilenamePath (\(Filename _ _ _ _ (Time _ _ _ _ s1 s2) _) -> (digitC #) <$> [s1, s2])
