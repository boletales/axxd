{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Control.Category ((>>>))
import qualified Data.List as L
import System.IO
import Data.Maybe (fromMaybe)

import Struct
import TypeParser
import Options
import Printer

data Args = Args
  { argTypeStr :: Maybe String
  , argOptions :: Options
  , argInputFile :: Maybe FilePath
  , argOutFile :: Maybe FilePath
  }

dumpaxxd :: Args -> String -> BSL.ByteString -> IO ()
dumpaxxd args typeStr content = do
  out <- 
    case argOutFile args of
      Nothing -> pure stdout
      Just outFilePath -> openFile outFilePath WriteMode
  case parseStructTypeFromString typeStr of
    Left err -> putStrLn ("Error parsing type: " ++ err)
    Right st -> prettyStruct out (argOptions args) st content

-- axxd [-s STRUCT_TYPE] [-blhd] INPUT_FILE [OUTPUT_FILE]

helpStrStructType :: String
helpStrStructType = 
  "Basic types:\n"
    ++ "  u8, ..., u64, i8, ..., i64, char\n"
    ++ "    - Basic value types\n\n"
    ++ "  str100 or s100\n"
    ++ "    - A string of 100 bytes\n\n"
    ++ "  pad100 or p100\n"
    ++ "    - 100 bytes of padding. Printed as hex\n\n"
    ++ "  <type>[100]\n"
    ++ "    - An array of 100 elements of the given type\n\n"
    ++ "  <type>[*]\n"
    ++ "    - An infinite array of the given type (prints until end of file)\n\n"
    ++ "  {<type>, <type>, field_name: <type>}\n"
    ++ "    - A struct with unnamed / named fields\n\n"
    ++ "  <type>l or <type>L\n"
    ++ "    - Print the type inline\n\n"
    ++ "Examples:\n"
    ++ "  u32[4]l[*]\n"
    ++ "    - Prints four u32 values on each line, until end of file\n\n"
    ++ "  {header: u32, payload: {name: str16, age: u8}l[*]}\n"
    ++ "    - Prints header, then payload\n\n"

argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
        ( long "struct-type"
       <> short 's'
       <> metavar "TYPE"
     <> help ("Struct settings. default: u32[4]l[*]")
     ))
  <*> (Options
        <$> flag LittleEndian BigEndian
            ( long "big-endian"
           <> short 'b'
           <> help "Use big-endian byte order" )
        <*> flag IntHex IntDec
            ( long "int-dec"
           <> short 'd'
           <> help "Display integers in decimal format" )
        <*> flag False True
            ( long "help-struct"
          <> help "Show help for struct type syntax" )
        <*> flag False True
          ( long "stdin"
          <> help "Read input from stdin instead of INPUT_FILE" ))
  <*> optional (strArgument
        ( metavar "INPUT_FILE"
         <> help "Input file to dump (omit when using --stdin)" ))
  <*> optional (strArgument
        ( metavar "OUTPUT_FILE"
       <> help "Output file to write dump to (defaults to stdout)" ))

main :: IO ()
main = do
  args <- execParser $ info (argsParser <**> helper)
    ( fullDesc
   <> progDesc "Dump binary files in a structured hex+ASCII format. See --help-struct for struct type syntax help."
   <> header "axxd - structured hexdump tool" )
  
  if optSeeStructHelp (argOptions args)
    then putStrLn helpStrStructType
    else do
      case (optUseStdin (argOptions args), argInputFile args) of
        (True, Just _) ->
          putStrLn "Cannot use --stdin and INPUT_FILE at the same time. Use --help for usage information."
        (True, Nothing) -> do
          content <- BSL.getContents
          let typeStr = fromMaybe "u32[4]l[*]" (argTypeStr args)
          dumpaxxd args typeStr content
        (False, Nothing) ->
          putStrLn "No input file specified. Use --stdin or see --help for usage information."
        (False, Just inputFilePath) -> do
          content <- BSL.readFile inputFilePath
          let typeStr = fromMaybe "u32[4]l[*]" (argTypeStr args)
          dumpaxxd args typeStr content