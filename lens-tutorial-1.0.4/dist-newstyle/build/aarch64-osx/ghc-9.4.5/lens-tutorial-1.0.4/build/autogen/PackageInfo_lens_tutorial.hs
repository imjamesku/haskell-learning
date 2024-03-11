{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_lens_tutorial (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "lens_tutorial"
version :: Version
version = Version [1,0,4] []

synopsis :: String
synopsis = "Tutorial for the lens library"
copyright :: String
copyright = "2015 Gabriella Gonzalez"
homepage :: String
homepage = ""
