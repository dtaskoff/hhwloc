{-# Language NamedFieldPuns #-}
import Control.Monad (unless)
import Data.List (intercalate)
import Distribution.Simple.InstallDirs
  ( InstallDirs(..), combinePathTemplate, fromPathTemplate, toPathTemplate
  , initialPathTemplateEnv, substituteInstallDirTemplates
  )
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey, localPackage, localUnitId)
import Distribution.Simple (compilerInfo, defaultMainWithHooks, preBuild, postReg, simpleUserHooks)
import System.Directory (doesFileExist)
import System.Process (CreateProcess(cwd), readCreateProcess, shell)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      let process command = putStrLn =<< readCreateProcess (shell command) { cwd = Just "./hwloc" } ""
          configuration =
            [ "--enable-static"
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

      autogend <- doesFileExist "hwloc/configure"
      configured <- (autogend &&) <$> doesFileExist "hwloc/Makefile"
      made <- (configured &&) <$> doesFileExist "hwloc/hwloc/.libs/libhwloc.a"

      unless autogend $ process "sh autogen.sh"
      unless configured $ process $ intercalate " " $ "sh configure" : configuration
      unless made $ process "make -C hwloc LDFLAGS=-all-static"

      pure emptyHookedBuildInfo

  , postReg = \_ _ _ lbi@LocalBuildInfo {compiler, hostPlatform, installDirTemplates} -> do
      let pathTemplateEnv = initialPathTemplateEnv (localPackage lbi) (localUnitId lbi) (compilerInfo compiler) hostPlatform
          InstallDirs { libdir, libsubdir } = substituteInstallDirTemplates pathTemplateEnv installDirTemplates
          libDir = combinePathTemplate libdir libsubdir
          libName = toPathTemplate $ "libHS" <> localCompatPackageKey lbi <> ".a"
          libPath = fromPathTemplate $ combinePathTemplate libDir libName
          libtool = intercalate " " [ "libtool -static -o", libPath, libPath, "hwloc/hwloc/.libs/libhwloc.a" ]

      putStrLn =<< readCreateProcess (shell libtool) ""
  }
