module Main where

import System.Directory
import System.Environment
import System.IO

import Data.Either
import Data.List (isInfixOf, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils
import Data.Version (showVersion)
import Distribution.Query
import Distribution.PackageDescription (GenericPackageDescription)


import qualified Paths_cabalQuery as CQ

main :: IO ()
main = do
  args <- getArgs
  eCfg <- parseArgs args
  case eCfg of
    Nothing -> printHelp
    Just cfg | ["--help"] `isInfixOf` (options cfg)    -> printHelp
             | ["--version"] `isInfixOf` (options cfg) -> printVersion
             | otherwise -> do
      let cmds = map cmdFn $ commands cfg
      case files cfg of
        []       -> do putStrLn "No .cabal files specified."
                       printHelp
        theFiles -> mapM_ (query cmds) theFiles

printVersion :: IO ()
printVersion = putStrLn ("cabalQuery-"++ showVersion CQ.version)

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: cabalQuery [command] [cabalfile] [options]"
  putStrLn ""
  putStrLn "Commands, files and options can be interspersed."
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help             Prints this help output."
  putStrLn "  --version          Nothing, yet."
  putStrLn "  --numeric-version  Nothing, yet."
  putStrLn ""
  putStrLn "Commands:"
  mapM_ (\cmd -> putStrLn ("  " ++ (cmdName cmd) ++ "\t\t" ++ (cmdDesc cmd))) commandList

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

data Config = Config { files :: [FilePath]
                     -- ^ The cabal file(s) to examine. Defaults to ./*.cabal
                     , commands :: [Command]
                     -- ^ The things to show, from the cabal file.
                     -- e.g., 'numeric-version'
                     , options :: [String]
                     -- ^ Any options to pass to cabalQuery, starting with "--"
                     }

toCommand :: String -> Either String Command
toCommand str = case Map.lookup str commandMap of
                  Nothing  -> Left str
                  Just cmd -> Right cmd

parseArgs :: [String] -> IO (Maybe Config)
parseArgs args = do
  let eCmds = map toCommand $ filter isCommand args
  case (lefts eCmds, rights eCmds) of
    ([] , cmds) -> do
      theFiles <- case filter isCabalFile args of
        [] -> do fs <- getCurrentDirectory >>= getDirectoryContents
                 return (filter isCabalFile fs)
        fs -> return fs
      return (Just (Config { files = theFiles
                           , options = filter isOption args
                           , commands = cmds
                           }))
    (errs, _) -> do
      hPutStrLn stderr "Invalid commands specified:"
      mapM_ (hPrint stderr) errs
      return Nothing

isCabalFile :: FilePath -> Bool
isCabalFile file = endswith ".cabal" file

isOption :: String -> Bool
isOption str = startswith "--" str

isCommand :: String -> Bool
isCommand str = not (isOption str || isCabalFile str)


query :: [GenericPackageDescription -> String] -> FilePath -> IO ()
query cmds file = do
  eGpb <- loadDescr file
  case eGpb of
    Left  err -> hPrint stderr err
    Right gpb -> putStrLn $ intercalate ", " (map ($ gpb) cmds)
