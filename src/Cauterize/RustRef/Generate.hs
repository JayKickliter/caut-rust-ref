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


dd :: T.Text
dd = "#[derive(Debug)]"


rustSrcFromSpec :: S.Specification -> T.Text
rustSrcFromSpec s = [str|
pub mod $modName$ {
#rt in rustTypes:$rt$|$endline$#}
|]
  where
    modName = S.specName s
    cautTypes = S.specTypes s
    rustTypes = removeEmptyStrings $ fmap (indent . cautToRustType) cautTypes


cautToRustType :: S.Type -> T.Text
cautToRustType t =
  case S.typeDesc t of
    S.Array{}       -> cautArrayToRustSlice t
    S.Combination{} -> T.empty
    S.Enumeration{} -> cautEnumerationToRustEnum t
    S.Range{}       -> T.empty
    S.Record{}      -> cautRecToRustStruct t
    S.Synonym{}     -> cautSynonymToRustNewtype t
    S.Union{}       -> cautUnionToRustEnum t
    S.Vector{}      -> cautVectorToRustVec t


cautRecToRustStruct :: S.Type -> T.Text
cautRecToRustStruct t = [str|
$dd$
pub struct $nm$ {
#field in fields:    pub $field$|$endline$#
}
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t


cautEnumerationToRustEnum :: S.Type -> T.Text
cautEnumerationToRustEnum t = [str|
$dd$
pub enum $nm$ {
#field in fields:    $field$|$endline$#
}
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t


cautArrayToRustSlice :: S.Type -> T.Text
cautArrayToRustSlice t = [str|
$dd$
pub struct $nm$([$elType$; $sz$]);
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    td = S.typeDesc t
    et = S.arrayRef td
    elType = cautNameToRustName et
    sz = T.pack . show . S.arrayLength $  td


cautVectorToRustVec :: S.Type -> T.Text
cautVectorToRustVec t = [str|
$dd$
pub struct $nm$(Vec<$elType$>);
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    td = S.typeDesc t
    et = S.vectorRef td
    elType = cautNameToRustName et


cautUnionToRustEnum :: S.Type -> T.Text
cautUnionToRustEnum t = [str|
$dd$
pub enum $nm$ {
#field in fields:    $field$|$endline$#
}
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t

cautSynonymToRustNewtype :: S.Type -> T.Text
cautSynonymToRustNewtype t = [str|
$dd$
pub struct $nm$($st$);
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    td = S.typeDesc t
    sr = S.synonymRef td
    st = cautNameToRustName sr


cautFieldToRustRecordField :: S.Field -> T.Text
cautFieldToRustRecordField (S.DataField n i r) =
  [str|$nm$: $fieldType$, // caut index = $idx$|]
  where
    nm = C.unIdentifier n
    fieldType = cautNameToRustName r
    idx = T.pack . show $ i
cautFieldToRustRecordField (S.EmptyField n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm = C.unIdentifier n
    idx = T.pack . show $ i


cautFieldToRustEnumField :: S.Field -> T.Text
cautFieldToRustEnumField (S.DataField n i r) =
  [str|$nm$($fieldType$), // caut index = $idx$|]
  where
    nm = titleCase . C.unIdentifier $  n
    fieldType = cautNameToRustName r
    idx = T.pack . show $ i
cautFieldToRustEnumField (S.EmptyField n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm = titleCase . C.unIdentifier $ n
    idx = T.pack . show $ i


cautEnumerationValToRustEnumField :: S.EnumVal -> T.Text
cautEnumerationValToRustEnumField (S.EnumVal n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm = titleCase . C.unIdentifier $ n
    idx = T.pack . show $ i


cautFieldsToRustFields :: S.Type -> [T.Text]
cautFieldsToRustFields S.Type {S.typeDesc = td} =
  case td of
    S.Record fs      -> fmap cautFieldToRustRecordField fs
    S.Union  fs _    -> fmap cautFieldToRustEnumField fs
    S.Enumeration  evs _    -> fmap cautEnumerationValToRustEnumField evs
    S.Combination {} -> error "Unimplemented"
    _                -> error "How did I get here?"
