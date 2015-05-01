module Distribution.Query where

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

loadDescr :: FilePath -> IO ()
loadDescr cabalFile = do
  resGpd <- parsePackageDescription cabalFile
  case resGpd of
    ParseFailed err -> putStrLn err
    ParseOk _   gpd -> putStrLn (versionBranch $ pkgVersion $ packageDescription gpd)
