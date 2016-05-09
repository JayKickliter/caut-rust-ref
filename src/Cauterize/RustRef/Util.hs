{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Util
  (specNameToRustName
  ,indent
  ,removeEmptyStrings
  ) where


import qualified Data.Text as T

specNameToRustName :: T.Text -> T.Text
specNameToRustName n = T.concat $ map T.toTitle $ T.split (== '_') n

indent :: T.Text -> T.Text
indent t = T.unlines $ T.append "    " <$> T.lines t

removeEmptyStrings :: [T.Text] -> [T.Text]
removeEmptyStrings = filter (not . (T.empty ==))
