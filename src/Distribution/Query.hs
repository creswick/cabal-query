module Distribution.Query where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version

loadDescr :: FilePath -> IO ()
loadDescr cabalFile = do
  content <- readFile cabalFile
  case parsePackageDescription content of
    ParseFailed err -> print err
    ParseOk _   gpd -> print (versionBranch $ pkgVersion $ package $ packageDescription gpd)
