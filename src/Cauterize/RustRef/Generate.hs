{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Cauterize.RustRef.Generate
       (spec2rust
       ) where

import qualified Cauterize.CommonTypes     as C
import           Cauterize.RustRef.Util
import qualified Cauterize.Specification   as S
import           Data.String.Interpolation
import qualified Data.Text                 as T


dd :: T.Text
dd = "#[derive(Debug)]"


spec2rust :: S.Specification -> T.Text
spec2rust s = [str|
pub mod $modName$ {
#rt in rustTypes:$rt$|$endline$#}$endline$
|]
  where
    modName   = S.specName s
    cautTypes = S.specTypes s
    rustTypes = nonEmpty $ fmap (indent . cautToRustType) cautTypes


cautToRustType :: S.Type -> T.Text
cautToRustType t =
  case S.typeDesc t of
    S.Array{}       -> array2slice t
    S.Combination{} -> combination2struct t
    S.Enumeration{} -> enumeration2enum t
    S.Range{}       -> range2unimplemented t
    S.Record{}      -> record2struct t
    S.Synonym{}     -> synonym2newtype t
    S.Union{}       -> union2enum t
    S.Vector{}      -> vector2vec t


range2unimplemented :: S.Type -> T.Text
range2unimplemented t = [str|
// Not declaring range $nm$.
// Range type not yet implemented.
|]
  where
    nm = cautNameToRustName . S.typeName $ t


combination2struct :: S.Type -> T.Text
combination2struct t = [str|
$dd$
pub struct $nm$ {
#field in fields:    pub $field$|$endline$#
}
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t


record2struct :: S.Type -> T.Text
record2struct t = [str|
$dd$
pub struct $nm$ {
#field in fields:    pub $field$|$endline$#
}
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t


enumeration2enum :: S.Type -> T.Text
enumeration2enum t = [str|
$dd$
pub enum $nm$ {
#field in fields:    $field$|$endline$#
}
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t


array2slice :: S.Type -> T.Text
array2slice t = [str|
$dd$
pub struct $nm$([$elType$; $sz$]);
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    td     = S.typeDesc t
    et     = S.arrayRef td
    elType = cautNameToRustName et
    sz     = T.pack . show . S.arrayLength $  td


vector2vec :: S.Type -> T.Text
vector2vec t = [str|
$dd$
pub struct $nm$(Vec<$elType$>);
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    td     = S.typeDesc t
    et     = S.vectorRef td
    elType = cautNameToRustName et


union2enum :: S.Type -> T.Text
union2enum t = [str|
$dd$
pub enum $nm$ {
#field in fields:    $field$|$endline$#
}
|]
  where
    nm     = cautNameToRustName . S.typeName $ t
    fields = cautFieldsToRustFields t

synonym2newtype :: S.Type -> T.Text
synonym2newtype t = [str|
$dd$
pub struct $nm$($st$);
|]
  where
    nm = cautNameToRustName . S.typeName $ t
    td = S.typeDesc t
    sr = S.synonymRef td
    st = cautNameToRustName sr


fiedl2structField :: S.Field -> T.Text
fiedl2structField (S.DataField n i r) =
  [str|$nm$: $fieldType$, // caut index = $idx$|]
  where
    nm        = C.unIdentifier n
    fieldType = cautNameToRustName r
    idx       = T.pack . show $ i
fiedl2structField (S.EmptyField n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm  = C.unIdentifier n
    idx = T.pack . show $ i


field2enumField :: S.Field -> T.Text
field2enumField (S.DataField n i r) =
  [str|$nm$($fieldType$), // caut index = $idx$|]
  where
    nm        = titleCase . C.unIdentifier $  n
    fieldType = cautNameToRustName r
    idx       = T.pack . show $ i
field2enumField (S.EmptyField n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm  = titleCase . C.unIdentifier $ n
    idx = T.pack . show $ i


comboField2structField :: S.Field -> T.Text
comboField2structField (S.DataField n i r) =
  [str|$nm$: Option<$fieldType$>, // caut index = $idx$|]
  where
    nm        = C.unIdentifier n
    fieldType = cautNameToRustName r
    idx       = T.pack . show $ i
comboField2structField (S.EmptyField n i) =
  [str|$nm$: Option<()>, // caut idx = $idx$|]
  where
    nm  = C.unIdentifier n
    idx = T.pack . show $ i


enumVal2enumField :: S.EnumVal -> T.Text
enumVal2enumField (S.EnumVal n i) =
  [str|$nm$, // caut idx = $idx$|]
  where
    nm  = titleCase . C.unIdentifier $ n
    idx = T.pack . show $ i


cautFieldsToRustFields :: S.Type -> [T.Text]
cautFieldsToRustFields S.Type {S.typeDesc = td} =
  case td of
    S.Record fs          -> fmap fiedl2structField fs
    S.Union  fs _        -> fmap field2enumField fs
    S.Enumeration  evs _ -> fmap enumVal2enumField evs
    S.Combination fs _   -> fmap comboField2structField fs
    _                    -> error "How did I get here?"
