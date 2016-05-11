{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Util
  (cautNameToRustName
  ,nonEmpty
  ,titleCase
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

cautNameToRustName :: C.Identifier -> String
cautNameToRustName i = if isCautPrim i
  then T.unpack . cautPrimToRustPrim $ i
  else T.unpack . titleCase . C.unIdentifier $ i

nonEmpty :: [T.Text] -> [T.Text]
nonEmpty = filter (not . (T.empty ==))
