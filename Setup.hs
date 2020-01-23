import Control.Monad (unless)
import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, preBuild)
import System.Directory (doesFileExist)
import System.Process (CreateProcess(cwd), readCreateProcess, shell)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
      let process command = putStrLn =<< readCreateProcess (shell command) { cwd = Just "./hwloc" } ""

      autogend <- doesFileExist "hwloc/configure"
      configured <- (autogend &&) <$> doesFileExist "hwloc/Makefile"
      made <- (configured &&) <$> doesFileExist "hwloc/hwloc/.libs/libhwloc.a"

      unless autogend $ process "./autogen.sh"
      unless configured $ process "./configure --enable-static --disable-shared"
      unless made $ process "make LDFLAGS=-all-static"

      pure emptyHookedBuildInfo
  }
