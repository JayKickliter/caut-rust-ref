{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Cauterize.RustRef.Generate
       (rustSrcFromSpec
       ) where

import qualified Cauterize.CommonTypes     as C
import           Cauterize.RustRef.Util
import qualified Cauterize.Specification   as S
import           Data.String.Interpolation
import qualified Data.Text                 as T

specNameText :: S.Type -> T.Text
specNameText = C.unIdentifier . S.typeName

dd :: T.Text
dd = "#[derive(Debug)]"

rustSrcFromSpec :: S.Specification -> T.Text
rustSrcFromSpec s = [str|
mod $modName$ {
#rt in rustTypes:$rt$|$endline$#}
|]
  where
    modName = S.specName s
    cautTypes = S.specTypes s
    rustTypes = removeEmptyStrings $ fmap (indent . cautToRustType) cautTypes

cautToRustType :: S.Type -> T.Text
cautToRustType t =
  case S.typeDesc t of
    S.Record _ -> cautRecToRustStruct t
    S.Enumeration _ _ -> cautEnumToRustEnum t
    _ -> T.empty

cautRecToRustStruct :: S.Type -> T.Text
cautRecToRustStruct t = [str|
$dd$
pub struct $nm$ {
    // fields
}
|]
  where
    nm = specNameToRustName . specNameText $ t

cautEnumToRustEnum :: S.Type -> T.Text
cautEnumToRustEnum t = [str|
$dd$
pub enum $nm$ {
    // fields
}
|]
  where
    nm = specNameToRustName . specNameText $ t
