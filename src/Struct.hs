module Struct
  ( ValueType(..)
  , StructType(..)
  , structSize
  , Splittable(..)
  , destructLv
  , destructLvM
  , isInline
  , fireOnInline
  , fireOnValue
  , fireOnMono
  , Context(..)
  , emptyContext
  ) where

import qualified Data.List as L
import Control.Monad.Writer.CPS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (when)


data ValueType =
    VChar
  | VUInt8
  | VUInt16
  | VUInt32
  | VUInt64
  | VInt8
  | VInt16
  | VInt32
  | VInt64
  | VPadding Int
  | VString  Int

instance Show ValueType where
  show VChar        = "char"
  show VUInt8       = "u8"
  show VUInt16      = "u16"
  show VUInt32      = "u32"
  show VUInt64      = "u64"
  show VInt8        = "i8"
  show VInt16       = "i16"
  show VInt32       = "i32"
  show VInt64       = "i64"
  show (VString n)  = "str" ++ show n
  show (VPadding n) = "pad" ++ show n

data StructType = 
    Mono ValueType
  | Array (Maybe Int) StructType
  | Struct [(Maybe String, StructType)]
  | Inline StructType

showStructItem :: (Maybe String, StructType) -> String
showStructItem (Nothing, st) = show st
showStructItem (Just name, st) = name ++ ": " ++ show st

instance Show StructType where
  show (Mono vt) = show vt
  show (Array Nothing st) = show st ++ "[*]"
  show (Array (Just n) st) = show st ++ "[" ++ show n ++ "]"
  show (Struct sts) = "{" ++ L.intercalate ", " (map showStructItem sts) ++ "}"
  show (Inline st) = "inline " ++ show st

valueTypeSize :: ValueType -> Int
valueTypeSize vt =
  case vt of
    VChar        -> 1
    VUInt8        -> 1
    VUInt16       -> 2
    VUInt32       -> 4
    VUInt64       -> 8
    VInt8        -> 1
    VInt16       -> 2
    VInt32       -> 4
    VInt64       -> 8
    VPadding n -> n
    VString n  -> n

structSize :: StructType -> Maybe Int
structSize struct =
  case struct of
    (Mono vt) -> Just (valueTypeSize vt)
    (Array (Just n) st) -> (* n) <$> structSize st
    (Array Nothing _) -> Nothing
    (Struct sts) -> L.foldl (\a c -> liftA2 (+) a (structSize (snd c))) (Just 0) sts
    (Inline st) -> structSize st


class Splittable a where
  splittableSize :: a -> Int

  hasContent :: a -> Bool

  {- |
  partialSplit size content
  if size is larger than content size, return Nothing
  -}
  partialSplit :: Int -> a -> Maybe (a, a)

makePartialSplit :: Integral i => (a -> Int) -> (i -> a -> (a, a)) -> Int -> a -> Maybe (a, a)
makePartialSplit sizeFunc splitFunc size content =
  let (taken, rest) = splitFunc (fromIntegral size) content
  in if size > sizeFunc taken
      then Nothing
      else Just (taken, rest)

instance Splittable BS.ByteString where
  splittableSize = BS.length
  partialSplit = makePartialSplit BS.length BS.splitAt
  hasContent bs = not (BS.null bs)

instance Splittable BSL.ByteString where
  splittableSize = fromIntegral . BSL.length
  partialSplit = makePartialSplit (fromIntegral . BSL.length) BSL.splitAt
  hasContent bs = not (BSL.null bs)

data Context = Context {
    contextPath :: [Maybe String]
  , contextOffset :: Int
  } deriving (Show, Eq)

emptyContext :: Context
emptyContext = Context [] 0

appendContext :: Int -> Maybe String -> Context -> Context
appendContext offset mName (Context path old) =
  Context (mName : path) (offset + old)

destructLvM :: (Monad m, Splittable content) => (m () -> Context -> StructType -> content -> m ()) -> Context -> StructType -> content -> m ()
destructLvM func context struct content =
  let dump d = func (pure ()) context (Mono (VPadding (splittableSize d))) d
      next =
        case struct of
          Inline st -> destructLvM func context st content
          Mono   _  -> pure ()
          Array mn st ->
            let go d n i o =
                  case n of
                    Just 0 -> pure ()
                    _ -> let here chunk = destructLvM func (appendContext o (Just (show i)) context) st chunk
                         in case structSize st of
                              Nothing -> here d
                              Just size ->
                                case partialSplit size d of
                                  Just (chunk, rest) -> here chunk >> when (hasContent rest) (go rest ((\x -> x - 1) <$> n) (i + 1) (o + size))
                                  Nothing -> dump d
            in go content mn (0 :: Int) (0 :: Int)
          Struct sts ->
            let go d sinfos o =
                  case sinfos of
                    [] -> pure ()
                    ((sname, sinfo):sinfos'') ->
                      let here chunk = destructLvM func (appendContext o sname context) sinfo chunk
                      in case structSize sinfo of
                            Nothing -> here d
                            Just size ->
                              case partialSplit size d of
                                Just (chunk, rest) -> here chunk >> when (hasContent rest) (go rest sinfos'' (o + size))
                                Nothing -> dump d
            in go content sts 0
  in func next context struct content

destructLv :: (Monoid result, Splittable content) => (result -> Context -> StructType -> content -> result) -> Context -> StructType -> content -> result
destructLv func context struct content =
  execWriter $
    destructLvM (\next ctx st cnt -> tell (func (execWriter next) ctx st cnt)) context struct content

isInline :: StructType -> Bool
isInline (Inline _) = True
isInline _         = False

-- testing

data SliceRange = SliceRange {
    sliceStart :: Int,
    sliceUntil :: Int
  } deriving (Show, Eq)

instance Splittable SliceRange where
  splittableSize (SliceRange start end) = end - start
  partialSplit size (SliceRange start end) =
    let totalSize = end - start
    in if size > totalSize
        then Nothing
        else Just (SliceRange start (start + size), SliceRange (start + size) end)
  hasContent (SliceRange start end) = start < end

fireOnInline :: (Context -> StructType -> content -> result) -> (result -> Context -> StructType -> content -> result)
fireOnInline f next context st content =
  case st of
    Mono _ -> f context st content
    Inline innerSt -> f context innerSt content
    _              -> next

fireOnValue :: (Context -> ValueType -> content -> result) -> (result -> Context -> StructType -> content -> result)
fireOnValue f next context st content =
  case st of
    Mono vt -> f context vt content
    _       -> next

fireOnMono :: (Context -> StructType -> content -> result) -> (result -> Context -> StructType -> content -> result)
fireOnMono f next context st content =
  case st of
    Mono _ -> f context st content
    _      -> next
-- >>> splitRange (Just 10) (SliceRange 10 40)

testStruct :: StructType
testStruct = Struct [
      (Nothing, Mono (VPadding 8))
    , (Nothing, Array Nothing (Inline (Struct [
          (Just "flag", Mono VUInt32)
        , (Nothing, Mono VUInt32)
        , (Nothing, Mono VUInt64)
        , (Just "name", Mono (VString 12))
        ])))
  ]

showStRange :: Context -> StructType -> SliceRange -> String
showStRange context st (SliceRange s e) = 
  replicate (L.length (contextPath context) * 2) ' ' ++ show st ++ " [" ++ show s ++ ", " ++ show e ++ ")\n"

testDestruct1 :: String
testDestruct1 = destructLv (fireOnInline showStRange) emptyContext testStruct (SliceRange 0 400)

testDestruct2 :: String
testDestruct2 = destructLv (fireOnMono showStRange) emptyContext testStruct (SliceRange 0 400)