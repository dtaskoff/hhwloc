{-# options_ghc -Wall -Werror -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates #-}
{-# Language NamedFieldPuns #-}

import Control.Monad (unless)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple (defaultMainWithHooks, preBuild, postBuild, simpleUserHooks)
import Distribution.Simple.InstallDirs (combinePathTemplate, fromPathTemplate, toPathTemplate)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Simple.Utils (findFileWithExtension, maybeExit, rawSystemExit, rawSystemIOWithEnv)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey)
import Distribution.Verbosity (normal)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ flags -> do
      let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
          rawSystemIO command args =
            maybeExit $
              rawSystemIOWithEnv verbosity command args (Just "hwloc") Nothing Nothing Nothing Nothing
          doesFileExist file = maybe False (const True) <$> findFileWithExtension [""] ["hwloc"] file

      autogend <- doesFileExist "configure"
      configured <- (autogend &&) <$> doesFileExist "Makefile"
      made <- (configured &&) <$> doesFileExist "hwloc/.libs/libhwloc.a"

      unless autogend $ rawSystemIO "sh" ["autogen.sh"]
      unless configured $ rawSystemIO "sh"
        [ "configure"
        , "--enable-static"
        , "--disable-shared"
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
      unless made $ rawSystemIO "make" ["-C", "hwloc", "LDFLAGS=-all-static"]

      pure emptyHookedBuildInfo

  , postBuild = \_ BuildFlags { buildVerbosity } _ lbi@LocalBuildInfo { buildDir } -> do
      let libName = toPathTemplate $ "libHS" <> localCompatPackageKey lbi <> ".a"
          libPath = fromPathTemplate $ combinePathTemplate (toPathTemplate buildDir) libName
          verbosity = fromFlagOrDefault normal buildVerbosity
       in rawSystemExit verbosity "libtool" ["-static", "-o", libPath, libPath, "hwloc/hwloc/.libs/libhwloc.a"]
  }
