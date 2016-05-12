{-# LANGUAGE OverloadedStrings #-}

module Cauterize.RustRef.Generate
       (spec2rust
       ) where

import qualified Cauterize.CommonTypes   as C
import           Cauterize.RustRef.Util
import qualified Cauterize.Specification as S
import           Data.Maybe
import qualified Data.Text               as T
import           Text.PrettyPrint.Leijen


dd :: Doc
dd = s "#[derive(Debug)]"

s :: String -> Doc
s = string

t :: T.Text -> Doc
t = s . T.unpack

spec2rust :: S.Specification -> T.Text
spec2rust = T.pack . source

source :: S.Specification -> String
source spec = renderDoc $ vcat $ punctuate empty
  [ s "pub mod" <+> specName <+> lbrace
  , vcat [ indent 4 (dd <> linebreak <> rustType tp) <> linebreak
         | tp <-  S.specTypes spec
         ]
  , rbrace
  , empty
  ]
  where
    specName = t . S.specName $ spec


renderDoc :: Doc -> String
renderDoc d = displayS (renderPretty 0.4 80 d) ""

rustType :: S.Type -> Doc
rustType tp = case S.typeDesc tp of
    S.Array{}       -> array2slice tp
    S.Combination{} -> combination2struct tp
    S.Enumeration{} -> enumeration2enum tp
    S.Range{}       -> range2unimplemented tp
    S.Record{}      -> record2struct tp
    S.Synonym{}     -> synonym2newtype tp
    S.Union{}       -> union2enum tp
    S.Vector{}      -> vector2vec tp


vec :: Doc -> Doc
vec d = s "Vec" <> angles d


newType :: Doc -> Doc -> Doc
newType nm tp = s "pub struct"
            <+> nm
            <>  parens tp
            <>  semi


enum :: Doc -> [Doc] -> Doc
enum nm fields = vcat
  [s "pub enum" <+> nm <+> lbrace
  , indent 4 (vcat fields)
  , rbrace
  ]


struct :: Doc -> [Doc] -> Doc
struct nm fields = vcat
  [s "pub struct" <+> nm <+> lbrace
  , indent 4 (vcat fields)
  , rbrace
  ]


comment :: Doc -> Doc
comment d = s "//" <+> d


range2unimplemented :: S.Type -> Doc
range2unimplemented tp = vcat
  [ comment $ s "Range type not yet implemented."
  , comment $ s "Not declaring range" <+> nm
  ]
  where
    nm = s. cautNameToRustName . S.typeName $ tp


combination2struct :: S.Type -> Doc
combination2struct tp = struct nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = cautFieldsToRustFields tp

record2struct :: S.Type -> Doc
record2struct tp = struct nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = cautFieldsToRustFields tp


enumeration2enum :: S.Type -> Doc
enumeration2enum tp = enum nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = cautFieldsToRustFields tp


array2slice :: S.Type -> Doc
array2slice tp = s "pub struct"
             <+> nm
              <> (parens . brackets $ elType <> semi <+> sz)
              <> semi
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    td     = S.typeDesc tp
    elType = s . cautNameToRustName . S.arrayRef $  td
    sz     = s . show . S.arrayLength $ td



vector2vec :: S.Type -> Doc
vector2vec tp = newType nm (vec elType)
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    td     = S.typeDesc tp
    elType = s . cautNameToRustName . S.vectorRef $  td



union2enum :: S.Type -> Doc
union2enum tp = enum nm fields
  where
    nm     = s . cautNameToRustName . S.typeName $ tp
    fields = cautFieldsToRustFields tp


synonym2newtype :: S.Type -> Doc
synonym2newtype tp = newType nm st
  where
    nm = s . cautNameToRustName . S.typeName $ tp
    td = S.typeDesc tp
    sr = S.synonymRef td
    st = s . cautNameToRustName $ sr


field2structField :: S.Field -> Doc
field2structField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> fieldType <> comma <+> comment idx
  where
    nm        = t . C.unIdentifier $  n
    fieldType = s . cautNameToRustName $ r
    idx       = s . show $ i
field2structField (S.EmptyField _ _) = empty


field2enumField :: S.Field -> Doc
field2enumField (S.DataField n i r) =
  nm <> parens fieldType <> comma <+> comment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    fieldType = s . cautNameToRustName $ r
    idx       = s . show $ i
field2enumField (S.EmptyField n i) =
  nm <> comma <+> comment idx
  where
    nm        = t . titleCase . C.unIdentifier $  n
    idx       = s . show $ i


option :: Maybe Doc -> Doc
option ot = s "Option" <> angles a
  where
   a = fromMaybe (s "()") ot


comboField2structField :: S.Field -> Doc
comboField2structField (S.DataField n i r) =
  s "pub" <+> nm <> colon <+> option fieldType <> comma <+> comment idx
  where
    nm        = t . C.unIdentifier $ n
    fieldType = Just . s . cautNameToRustName $ r
    idx       = s . show $ i
comboField2structField (S.EmptyField n i) =
  s "pub" <+> nm <> colon <+> option Nothing <> comma <+> comment idx
  where
    nm  = t . C.unIdentifier $ n
    idx = s . show $ i


enumVal2enumField :: S.EnumVal -> Doc
enumVal2enumField (S.EnumVal n i) = nm <> comma <+> comment idx
  where
    nm  = s . T.unpack . titleCase . C.unIdentifier $ n
    idx = s . show $ i


cautFieldsToRustFields :: S.Type -> [Doc]
cautFieldsToRustFields S.Type {S.typeDesc = td} =
  case td of
    S.Record fs          -> fmap field2structField fs
    S.Union  fs _        -> fmap field2enumField fs
    S.Enumeration  evs _ -> fmap enumVal2enumField evs
    S.Combination fs _   -> fmap comboField2structField fs
    _                    -> error "How did I get here?"
