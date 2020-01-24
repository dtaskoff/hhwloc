import Control.Monad (unless)
import Data.List (intercalate)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, preBuild)
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

      unless autogend $ process "./autogen.sh"
      unless configured $ process $ intercalate " " $ "./configure" : configuration
      unless made $ process "make -C hwloc LDFLAGS=-all-static"

      pure emptyHookedBuildInfo
  }
