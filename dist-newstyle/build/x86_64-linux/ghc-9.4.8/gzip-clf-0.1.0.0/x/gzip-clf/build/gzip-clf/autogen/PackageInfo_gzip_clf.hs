{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_gzip_clf (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "gzip_clf"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Implementation of a Parameter free text classification algorithm based on the GZip compression algorithm"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
