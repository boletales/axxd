module Printer (prettyStruct) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import Control.Category ((>>>))
import qualified Data.List as L
import System.IO
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32, Word64)
import Data.Binary.Get
import Numeric (showHex)

import Struct
import Options

prettyOffsetNumber :: Options -> Int -> BSB.Builder
prettyOffsetNumber _ offset =
  BSB.word32HexFixed (fromIntegral offset) <> BSB.string7 ":  "

prettyHex :: Options -> BS.ByteString -> Maybe Int -> BSB.Builder
prettyHex _ content intendedSize =
  let hexBytes = (map BSB.word8HexFixed >>> L.intersperse (BSB.char8 ' ') >>> mconcat) (BS.unpack content)
      padding =
        case intendedSize of
          Just size
            | BS.length content < size ->
                let missing = size - BS.length content
                    padHex = L.replicate missing (BSB.string7 "   ")
                in mconcat padHex
          _ -> mempty
  in hexBytes <> padding

prettyAscii :: Options -> BS.ByteString -> Maybe Int -> BSB.Builder
prettyAscii _ content intendedSize =
  let toAsciiChar b
        | b >= 32 && b <= 126 = BSB.char8 (toEnum (fromIntegral b))
        | otherwise           = BSB.char8 '.'
      asciiBytes = (map toAsciiChar >>> mconcat) (BS.unpack content)
      padding =
        case intendedSize of
          Just size
            | BS.length content < size ->
                let missing = size - BS.length content
                    padAscii = L.replicate missing (BSB.char8 ' ')
                in mconcat padAscii
          _ -> mempty
  in asciiBytes <> padding


prettyStructPadding :: Options -> BS.ByteString -> BSB.Builder
prettyStructPadding opt content =
  let padBytes = BS.length content
  in prettyHex opt content (Just padBytes)

prettyStructString :: Options -> BS.ByteString -> BSB.Builder
prettyStructString opt content = 
  let stripped = BS.takeWhile (/= 0) content
  in BSB.string7 "\"" <> prettyAscii opt stripped (Just (BS.length stripped)) <> BSB.string7 "\""

prettyStructInt :: Integral i => Get i -> Options -> BS.ByteString -> BSB.Builder
prettyStructInt intGetter opt content =
  let intVal = fromIntegral $ runGet intGetter (BSL.fromStrict content) 
      intStr = 
        case optIntFormat opt of
          IntDec -> BSB.string7 (show intVal)
          IntHex -> BSB.string7 ("0x" ++ showHex intVal "")  
  in intStr

prettyStructValue :: Options -> Context -> ValueType -> BS.ByteString -> BSB.Builder
prettyStructValue opt context vt content =
  let body =
        case optEndianness opt of
          LittleEndian ->
            case vt of
              VChar        -> prettyStructInt getInt8 opt content
              VUInt8       -> prettyStructInt getWord8 opt content
              VUInt16      -> prettyStructInt getWord16le opt content
              VUInt32      -> prettyStructInt getWord32le opt content
              VUInt64      -> prettyStructInt getWord64le opt content
              VInt8        -> prettyStructInt getInt8 opt content
              VInt16       -> prettyStructInt getInt16le opt content
              VInt32       -> prettyStructInt getInt32le opt content
              VInt64       -> prettyStructInt getInt64le opt content
              VPadding _   -> prettyStructPadding opt content
              VString _    -> prettyStructString opt content
          BigEndian ->
            case vt of
              VChar        -> prettyStructInt getInt8 opt content
              VUInt8       -> prettyStructInt getWord8 opt content
              VUInt16      -> prettyStructInt getWord16be opt content
              VUInt32      -> prettyStructInt getWord32be opt content
              VUInt64      -> prettyStructInt getWord64be opt content
              VInt8        -> prettyStructInt getInt8 opt content
              VInt16       -> prettyStructInt getInt16be opt content
              VInt32       -> prettyStructInt getInt32be opt content
              VInt64       -> prettyStructInt getInt64be opt content
              VPadding _   -> prettyStructPadding opt content
              VString _    -> prettyStructString opt content
      index = case contextPath context of
                (Just name : _) -> BSB.string7 (name ++ ": ")
                _               -> mempty
  in index <> body <> BSB.string7 " ,"

prettyStructLine :: Options -> Context -> StructType -> BS.ByteString -> BSB.Builder
prettyStructLine opt context st content =
  let size = structSize st
  in     prettyOffsetNumber opt (contextOffset context)
      <> prettyHex opt content size
      <> BSB.string7 "  |"
      <> prettyAscii opt content size
      <> BSB.string7 "|  "
      <> destructLv (fireOnValue (prettyStructValue opt)) context st content
      <> BSB.char8 '\n'

prettyStruct :: Handle -> Options -> StructType -> BSL.ByteString -> IO ()
prettyStruct out opt st content =
  destructLv (fireOnInline (\l s c -> BSB.hPutBuilder out (prettyStructLine opt l s (BSL.toStrict c)))) emptyContext st content