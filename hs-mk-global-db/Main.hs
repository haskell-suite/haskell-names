import Language.Haskell.Names.Interfaces
import Options.Applicative
import Data.Monoid
import Data.Proxy
import qualified Data.Map as Map
import Control.Monad
import Distribution.HaskellSuite
import Distribution.Simple.Compiler
import Distribution.Package
import Distribution.InstalledPackageInfo
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils (installDirectoryContents)
import Text.Printf
import System.Exit
import System.FilePath
import System.Directory

main =
  join $
    execParser $ flip info mempty $
      genDb <$>
        strOption (long "package-db") <*>
        strOption (short 'o')

pkgNames = map PackageName ["base", "ghc-prim", "integer-simple", "array"]

genDb :: FilePath -> FilePath -> IO ()
genDb pkgDb dest = do
  pkgs <- getInstalledPackages (Proxy :: Proxy NamesDB) (SpecificPackageDB pkgDb)
  
  let
    pkgMap :: Map.Map PackageName Packages
    pkgMap = Map.fromListWith mappend
      [ ((pkgName . sourcePackageId) pkg, [pkg]) | pkg <- pkgs ]

    mbPkgs :: Either PackageName Packages
    mbPkgs =
      fmap concat . forM pkgNames $ \pkgName ->
        maybe (Left pkgName) Right $ Map.lookup pkgName pkgMap
      
  case mbPkgs of
    Left pkg -> do
      printf "Core package %s is not installed in the database %s\n"
        (display pkg)
        dest
      exitFailure

    Right pkgs -> do
      createDirectoryIfMissing True dest
      pkgs' <- forM pkgs $ \pkg -> do
        let newDir = display (installedPackageId pkg)

        installDirectoryContents normal (head $ libraryDirs pkg) (dest </> newDir)

        return $ pkg { libraryDirs = [newDir] }

      writeDB (dest </> "packages.db") pkgs'
