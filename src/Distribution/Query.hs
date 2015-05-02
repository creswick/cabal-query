module Distribution.Query where

import Data.List
import Data.Version (showVersion)
import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Version

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO

import Text.PrettyPrint (render)

commandMap :: Map String Command
commandMap = Map.fromList $ map (\cmd -> (cmdName cmd, cmd)) commandList

commandList :: [Command]
commandList =
  [ Command "numeric-version"
      "Returns the numeric version in a machine-readable fashion." numericVersion
  , Command "version" "An alias for numeric-version" numericVersion
  , Command "name"  "The package name." name
  , Command "license" "The package license." license
  , Command "copyright" "The package copyright." copyright
  , Command "homepage" "The package homepage." homepage
  , Command "pkgUrl" "The package url." pkgUrl
  , Command "bugReports" "The bug reporting url." bugReports
  , Command "synopsis" "The package synopsis." synopsis
  , Command "description" "The package description." description
  , Command "buildType" "The buildType." buildType
  , Command "dataDir" "The dataDir." dataDir
  ]

data Command = Command { cmdName :: String
                         -- ^ The name of the command, as written on the command line.
                       , cmdDesc :: String
                         -- ^ A description of the command, for help output.
                       , cmdFn :: GenericPackageDescription -> String
                         -- ^ The function over a package description
                         -- to get the string value of the thing we're requesting.
                       }

query :: [GenericPackageDescription -> String] -> FilePath -> IO ()
query cmds file = do
  eGpb <- loadDescr file
  case eGpb of
    Left  err -> hPrint stderr err
    Right gpb -> putStrLn $ intercalate ", " (map ($ gpb) cmds)

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
