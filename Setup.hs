{-# options_ghc -Wall -Werror -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates #-}
{-# Language BlockArguments #-}
{-# Language NamedFieldPuns #-}

import Control.Monad (unless)
import Data.List (isSuffixOf)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription (PackageDescription(..), emptyHookedBuildInfo, explicitLibModules)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (defaultMainWithHooks, preBuild, postBuild, simpleUserHooks)
import Distribution.Simple.Compiler (compilerVersion)
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Types.InstalledPackageInfo (InstalledPackageInfo(..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey)
import Distribution.Verbosity (normal)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)
import System.Info (os)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ BuildFlags { buildVerbosity }  ->
      withCurrentDirectory "hwloc" do

        autogend <- doesFileExist "configure"
        configured <- (autogend &&) <$> doesFileExist "Makefile"
        made <- (configured &&) <$> doesDirectoryExist "hwloc/.libs"

        let verbosity = fromFlagOrDefault normal buildVerbosity
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

  , postBuild = \_ BuildFlags { buildVerbosity } PackageDescription { library } localBuildInfo ->
      case explicitLibModules <$> library of
        Nothing -> error "No library!"
        Just modules -> do
          hwlocLibs <- listDirectory "hwloc/hwloc/.libs"

          let verbosity = fromFlagOrDefault normal buildVerbosity
              LocalBuildInfo { buildDir, compiler, installedPkgs } = localBuildInfo

              hwlocObjects = [ "hwloc/hwloc/.libs" </> file | file <- hwlocLibs, ".o" `isSuffixOf` file ]
              hhwlocModulesPaths = map ((buildDir </>) . toFilePath) modules
              hhwlocObjects = map (<.> "o") hhwlocModulesPaths
              hhwlocDynObjects = map (<.> "dyn_o") hhwlocModulesPaths

              libHShwloc = "libHS" <> localCompatPackageKey localBuildInfo
              libPath = buildDir </> libHShwloc <.> "a"
              dynLibHShwloc = libHShwloc <> "-ghc" <> prettyShow (compilerVersion compiler)

              commonOptions = [ "-fno-stack-protector", "-DTABLES_NEXT_TO_CODE" ]
              rpaths = map ("-Wl,-rpath," <>) libDirs
              libraries = concat
                [ map ("-L" <>) libDirs
                , map ("-l" <>) $ concatMap hsLibraries pkgs ++ concatMap extraLibraries pkgs
                ]
              libDirs = concatMap libraryDirs pkgs
              pkgs = allPackages installedPkgs

          rawSystemExit verbosity "ar" $ ["-r", libPath] ++ hwlocObjects ++ hhwlocObjects
          -- ^ create a new static library with all of libhwloc's and hhwloc's objects

          case os of
            "darwin" ->
              rawSystemExit verbosity "gcc" $
                concat
                  [ commonOptions, rpaths
                  , [ "-dynamiclib", "-o", buildDir </> dynLibHShwloc <.> "dylib"
                    , "-undefined", "dynamic_lookup", "-single_module"
                    , "-install_name", "@rpath" </> dynLibHShwloc <.> "dylib"
                    ]
                  , hwlocObjects ++ hhwlocDynObjects, libraries, ["-Wl,-dead_strip_dylibs"]
                  ]
            "linux" ->
              rawSystemExit verbosity "gcc" $
                concat
                  [ commonOptions, ["-fuse-ld=gold", "-Wl,--no-as-needed"], rpaths
                  , [ "-o", buildDir </> dynLibHShwloc <.> "so", "-shared", "-Wl,-Bsymbolic"
                    , "-Wl,-h," <> dynLibHShwloc <.> "so"
                    ]
                  , hwlocObjects ++ hhwlocDynObjects, libraries
                  ]
            _ -> pure ()
          -- ^ create a new dynamic library with all of libhwloc's and hhwloc's objects
  }

(</>), (<.>) :: FilePath -> FilePath -> FilePath
l </> r = l <> "/" <> r
l <.> r = l <> "." <> r
