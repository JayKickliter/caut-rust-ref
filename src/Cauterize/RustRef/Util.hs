{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Util
  (cautTypeToRustType
  ,nonEmpty
  ,titleCase
  ,cautFieldToRustField
  ,cautTagToRustType
  ) where


import qualified Cauterize.CommonTypes as C
import qualified Data.Text             as T

cautPrimToRustPrim :: C.Identifier -> T.Text
cautPrimToRustPrim i = case C.unIdentifier i of
  "s8" -> "i8"
  "s16" -> "i16"
  "s32" -> "i32"
  "s64" -> "i64"
  _ -> C.unIdentifier i

titleCase :: T.Text -> T.Text
titleCase t = T.concat $ map T.toTitle $ T.split (== '_') t

isCautPrim :: C.Identifier -> Bool
isCautPrim i = i `elem` C.allPrimNames

cautTypeToRustType :: C.Identifier -> String
cautTypeToRustType i = if isCautPrim i
  then T.unpack . cautPrimToRustPrim $ i
  else T.unpack . titleCase . C.unIdentifier $ i

cautFieldToRustField :: C.Identifier -> String
cautFieldToRustField  = T.unpack . titleCase . C.unIdentifier

nonEmpty :: [T.Text] -> [T.Text]
nonEmpty = filter (not . (T.empty ==))

cautTagToRustType :: C.Tag -> String
cautTagToRustType tag = case tag of
  C.T1 -> "u8"
  C.T2 -> "u16"
  C.T4 -> "u32"
  C.T8 -> "u64"
