{-# options_ghc -Wall -Werror -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates #-}
{-# Language BlockArguments #-}
{-# Language NamedFieldPuns #-}

import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.List (isSuffixOf)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple (defaultMainWithHooks, preBuild, postBuild, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags(..), fromFlagOrDefault)
import Distribution.Simple.Utils (maybeExit, rawSystemIOWithEnv)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(..), localCompatPackageKey)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (createDirectory, doesFileExist, listDirectory, removeDirectoryRecursive)


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
      let libPath = buildDir </> "libHS" <> localCompatPackageKey lbi <.> "a"
          verbosity = fromFlagOrDefault normal buildVerbosity
          objDir = ".objhwloc"

      bracket_ (createDirectory objDir) (removeDirectoryRecursive objDir) do
        let rawSystemIO command args = rawSystemIOWithCwd verbosity command args $ Just objDir

        rawSystemIO "pwd" []
        rawSystemIO "ar" ["-x", "../hwloc/hwloc/.libs/libhwloc.a"]
        rawSystemIO "ar" ["-x", ".." </> libPath]
        objects <- filter (".o" `isSuffixOf`) <$> listDirectory ".objhwloc"
        rawSystemIO "ar" $ ["-r", ".." </> libPath] ++ objects
  }

(</>), (<.>) :: FilePath -> FilePath -> FilePath
l </> r = l <> "/" <> r
l <.> r = l <> "." <> r

rawSystemIOWithCwd :: Verbosity -> String -> [String] -> Maybe FilePath -> IO ()
rawSystemIOWithCwd verbosity command args cwd = maybeExit do
  rawSystemIOWithEnv verbosity command args cwd Nothing Nothing Nothing Nothing
