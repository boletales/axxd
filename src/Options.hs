module Options
  ( Endianness(..)
  , Options(..)
  , IntFormat(..)
  ) where

import System.IO

data Endianness =
    LittleEndian
  | BigEndian

data IntFormat =
    IntDec
  | IntHex
  deriving (Show, Eq)

data Options = Options {
    optEndianness :: Endianness
  , optIntFormat  :: IntFormat
  , optSeeStructHelp :: Bool
  }

