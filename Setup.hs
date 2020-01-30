{-# options_ghc -Wall -Werror -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates #-}
{-# Language BlockArguments #-}
{-# Language NamedFieldPuns #-}

import Control.Monad (unless)
import Data.List (isSuffixOf)
import Distribution.ModuleName (toFilePath)
import Distribution.PackageDescription (emptyHookedBuildInfo, explicitLibModules, library)
import Distribution.Simple (defaultMainWithHooks, preBuild, postBuild, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Simple.Utils (maybeExit, rawSystemIOWithEnv)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (doesFileExist, listDirectory)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ flags -> do
      let verbosity = fromFlagOrDefault normal $ buildVerbosity flags
          rawSystemIO command args = rawSystemIOWithCwd verbosity command args $ Just "hwloc"

      autogend <- doesFileExist "hwloc/configure"
      configured <- (autogend &&) <$> doesFileExist "hwloc/Makefile"
      made <- (configured &&) <$> doesFileExist "hwloc/hwloc/.libs/libhwloc.a"

      unless autogend $ rawSystemIO "sh" ["autogen.sh"]
      unless configured $ rawSystemIO "sh"
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
      unless made $ rawSystemIO "make" ["-C", "hwloc"]

      pure emptyHookedBuildInfo

  , postBuild = \_ buildFlags packageDescription localBuildInfo@LocalBuildInfo { buildDir } ->
      case explicitLibModules <$> library packageDescription of
        Nothing -> error "No library!"
        Just modules -> do
          hwlocLibs <- listDirectory "hwloc/hwloc/.libs"

          let libPath = buildDir </> "libHS" <> localCompatPackageKey localBuildInfo <.> "a"
              verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags

              hwlocObjects = [ "hwloc/hwloc/.libs" </> file | file <- hwlocLibs, ".o" `isSuffixOf` file ]
              hhwlocObjects = map ((buildDir </>) . (<.> "o") . toFilePath) modules

          let rawSystemIO command args = rawSystemIOWithCwd verbosity command args Nothing

          rawSystemIO "ar" $ ["-r", libPath] ++ hwlocObjects ++ hhwlocObjects
          -- ^ create a new static library with all of libhwloc's and hhwloc's objects
  }

(</>), (<.>) :: FilePath -> FilePath -> FilePath
l </> r = l <> "/" <> r
l <.> r = l <> "." <> r

rawSystemIOWithCwd :: Verbosity -> String -> [String] -> Maybe FilePath -> IO ()
rawSystemIOWithCwd verbosity command args cwd = maybeExit do
  rawSystemIOWithEnv verbosity command args cwd Nothing Nothing Nothing Nothing
