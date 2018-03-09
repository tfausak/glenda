-- | This module defines the version number of this library. It comes directly
-- from the private @Paths_glenda@ module. End users typically won't need to
-- use the version number in code. However it can be useful for debugging.
module Glenda.Version
  ( version
  , versionString
  ) where

import qualified Data.Version as Version
import qualified Paths_glenda as This

-- | The version of this library. This is unrelated to the version of Go, like
-- "1.10". It is also unrelated to the version of the spec, like "February 1,
-- 2018".
version :: Version.Version
version = This.version

-- | The canonical string representation of the version of this library.
versionString :: String
versionString = Version.showVersion version
