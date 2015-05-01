module Distribution.Query where

import Data.List
import Data.Version (showVersion)
import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Version

import Text.PrettyPrint (render)

loadDescr :: FilePath -> IO (Either String GenericPackageDescription)
loadDescr cabalFile = do
  content <- readFile cabalFile
  return $ case parsePackageDescription content of
    ParseFailed err -> Left (show err)
    ParseOk _   gpd -> Right gpd

numericVersion :: GenericPackageDescription -> String
numericVersion = showVersion . pkgVersion . PD.package . PD.packageDescription

name :: GenericPackageDescription -> String
name = unPackageName . pkgName . PD.package . PD.packageDescription

license :: GenericPackageDescription -> String
license = render . disp . PD.license . PD.packageDescription

copyright :: GenericPackageDescription -> String
copyright = getField PD.copyright

homepage :: GenericPackageDescription -> String
homepage = getField PD.homepage

pkgUrl :: GenericPackageDescription -> String
pkgUrl = getField PD.pkgUrl

bugReports :: GenericPackageDescription -> String
bugReports = getField PD.bugReports

category :: GenericPackageDescription -> String
category = getField PD.category

buildType :: GenericPackageDescription -> String
buildType = getField PD.buildType

dataDir :: GenericPackageDescription -> String
dataDir = getField PD.dataDir

synopsis :: GenericPackageDescription -> String
synopsis = getField PD.synopsis

description :: GenericPackageDescription -> String
description = getField PD.description



getField :: Show a => (PD.PackageDescription -> a) -> GenericPackageDescription -> String
getField getter = show . getter . PD.packageDescription
