{-# options_ghc -Wall -Werror -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates #-}
{-# Language BlockArguments #-}
{-# Language NamedFieldPuns #-}

import Control.Monad (unless)
import Data.List (isSuffixOf)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription (emptyHookedBuildInfo, explicitLibModules, library)
import Distribution.Simple (defaultMainWithHooks, preBuild, postBuild, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey)
import Distribution.Verbosity (normal)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ buildFlags ->
      withCurrentDirectory "hwloc" do

        autogend <- doesFileExist "configure"
        configured <- (autogend &&) <$> doesFileExist "Makefile"
        made <- (configured &&) <$> doesDirectoryExist "hwloc/.libs"

        let verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags
        unless autogend $ rawSystemExit verbosity "sh" ["autogen.sh"]
        unless configured $ rawSystemExit verbosity "sh"
          [ "configure"
          , "--enable-static"
          , "--disable-picky"
          , "--disable-cairo"
          , "--disable-cpuid"
          , "--disable-libxml2"
          , "--disable-io"
          , "--disable-pci"
          , "--disable-opencl"
          , "--disable-cuda"
          , "--disable-nvml"
          , "--disable-gl"
          , "--disable-libudev"
          , "--disable-netloc"
          ]
        unless made $ rawSystemExit verbosity "make" ["-C", "hwloc"]

        pure emptyHookedBuildInfo

  , postBuild = \_ buildFlags packageDescription localBuildInfo@LocalBuildInfo { buildDir } ->
      case explicitLibModules <$> library packageDescription of
        Nothing -> error "No library!"
        Just modules -> do
          hwlocLibs <- listDirectory "hwloc/hwloc/.libs"

          let libPath = buildDir </> "libHS" <> localCompatPackageKey localBuildInfo <.> "a"
              hwlocObjects = [ "hwloc/hwloc/.libs" </> file | file <- hwlocLibs, ".o" `isSuffixOf` file ]
              hhwlocObjects = map ((buildDir </>) . (<.> "o") . toFilePath) modules

              verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags

          rawSystemExit verbosity "ar" $ ["-r", libPath] ++ hwlocObjects ++ hhwlocObjects
          -- ^ create a new static library with all of libhwloc's and hhwloc's objects
  }

(</>), (<.>) :: FilePath -> FilePath -> FilePath
l </> r = l <> "/" <> r
l <.> r = l <> "." <> r
