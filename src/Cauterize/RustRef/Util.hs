{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Util
  (cautNameToRustName
  ,indent
  ,removeEmptyStrings
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


isCautPrim :: C.Identifier -> Bool
isCautPrim i = i `elem` C.allPrimNames

cautNameToRustName :: C.Identifier -> T.Text
cautNameToRustName i = if isCautPrim i
  then cautPrimToRustPrim i
  else T.concat $ map T.toTitle $ T.split (== '_') (C.unIdentifier i)

indent :: T.Text -> T.Text
indent t = T.unlines $ T.append "    " <$> T.lines t

removeEmptyStrings :: [T.Text] -> [T.Text]
removeEmptyStrings = filter (not . (T.empty ==))
