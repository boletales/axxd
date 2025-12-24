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
  , argInputFile :: FilePath
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

argsParser :: Parser Args
argsParser = Args
  <$> optional (strOption
        ( long "struct-type"
       <> short 's'
       <> metavar "TYPE"
       <> help "Struct type description string" ))
  <*> (Options
        <$> flag LittleEndian BigEndian
            ( long "big-endian"
           <> short 'b'
           <> help "Use big-endian byte order" )
        <*> flag IntHex IntDec
            ( long "int-dec"
           <> short 'd'
           <> help "Display integers in decimal format" ))
  <*> strArgument
        ( metavar "INPUT_FILE"
       <> help "Input file to dump" )
  <*> optional (strArgument
        ( metavar "OUTPUT_FILE"
       <> help "Output file to write dump to (defaults to stdout)" ))

main :: IO ()
main = do
  args <- execParser $ info (argsParser <**> helper)
    ( fullDesc
   <> progDesc "Dump binary files in a structured hex+ASCII format"
   <> header "axxd - structured hexdump tool" )
  content <- BSL.readFile (argInputFile args)
  let typeStr = fromMaybe "u32[4]l[*]" (argTypeStr args)
  dumpaxxd args typeStr content