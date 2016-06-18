{-# LANGUAGE TemplateHaskell #-}
module Cauterize.RustRef.Static
  ( allFiles
  ) where

import qualified Data.ByteString as B
import           Data.FileEmbed

cauterizeDotRs :: (FilePath, B.ByteString)
cauterizeDotRs = ("src/cauterize.rs", $(embedFile "static/cauterize.rs"))

allFiles :: [(FilePath, B.ByteString)]
allFiles =
  [ cauterizeDotRs
  ]
