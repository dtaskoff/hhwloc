{-# Language NamedFieldPuns #-}
import Control.Monad (unless)
import Data.List (intercalate)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple (compilerInfo, defaultMainWithHooks, preBuild, postReg, simpleUserHooks)
import Distribution.Simple.InstallDirs
  ( InstallDirs(..), combinePathTemplate, fromPathTemplate, toPathTemplate
  , initialPathTemplateEnv, substituteInstallDirTemplates
  )
import Distribution.Simple.Setup (BuildFlags(buildVerbosity), RegisterFlags(regVerbosity), fromFlagOrDefault)
import Distribution.Simple.Utils (findFileWithExtension, maybeExit, rawSystemExit, rawSystemIOWithEnv)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey, localPackage, localUnitId)
import Distribution.Verbosity (normal)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ flags -> do
      let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
          rawSystemIO command args =
            maybeExit $ rawSystemIOWithEnv verbosity command args (Just "hwloc") Nothing Nothing Nothing Nothing
          doesFileExist file = maybe False (const True) <$>
            findFileWithExtension [""] ["hwloc"] file

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

  , postReg = \_ flags _ lbi@LocalBuildInfo { compiler, hostPlatform, installDirTemplates } -> do
      let pathTemplateEnv = initialPathTemplateEnv (localPackage lbi) (localUnitId lbi) (compilerInfo compiler) hostPlatform
          InstallDirs { libdir, libsubdir } = substituteInstallDirTemplates pathTemplateEnv installDirTemplates
          libDir = combinePathTemplate libdir libsubdir
          libName = toPathTemplate $ "libHS" <> localCompatPackageKey lbi <> ".a"
          libPath = fromPathTemplate $ combinePathTemplate libDir libName
          verbosity = fromFlagOrDefault normal $ regVerbosity flags

      rawSystemExit verbosity "libtool" ["-static", "-o", libPath, libPath, "hwloc/hwloc/.libs/libhwloc.a"]
  }
