{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
  Description : Quasi-quoters for the JsonSpec language

  Quasi-quoters for the JsonSpec textual language.

  > type Person =
  >   [jsonspec|
  >     module Person = {
  >       "name": string,
  >       "age": int
  >     }
  >   |]

  produces a type of kind 'Module'.

  See @docs\/language-spec.md@.
-}
module Data.JsonSpec.Language.QQ (
  jsonspec,
) where

import Data.JsonSpec.Language.Parser
  ( Binding(ModuleBinding, TypeBinding)
  , Field(Field, fieldName, fieldOptional, fieldSpec)
  , Program(Program, programSpec)
  , Spec
    ( ArraySpec, BoolSpec, DateTimeSpec, DictSpec, EitherSpec, IntSpec, LetSpec
    , NullSpec, NumberSpec, ObjectSpec, RawSpec, RefSpec, StringSpec, TagSpec
    )
  , parseProgram
  )
import Data.JsonSpec.Spec
  ( BindingSpec(ModuleBind, TypeBind), FieldSpec(Optional, Required)
  , Module(Module)
  , Specification
    ( JsonArray, JsonBool, JsonDateTime, JsonDict, JsonEither, JsonInt, JsonLet
    , JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString, JsonTag
    )
  )
import Data.Text (Text)
import Language.Haskell.TH (Q, Type, TypeQ, appT, litT, promotedT, strTyLit)
import Language.Haskell.TH.Quote
  ( QuasiQuoter(QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType)
  )
import Prelude
  ( Bool(False, True), Either(Left, Right), Foldable(foldr), Functor(fmap)
  , MonadFail(fail), Semigroup((<>)), String
  )
import qualified Data.Text as T

{-|
  Quasi-quoter for a JsonSpec program.

  The quoted text must be a full program
  (@module Name = \<spec\>@). Use in type context; the result has
  kind 'Module'.
-}
jsonspec :: QuasiQuoter
jsonspec =
  QuasiQuoter
    { quoteExp  = unsupported "expression"
    , quotePat  = unsupported "pattern"
    , quoteType = quoteJsonSpecType
    , quoteDec  = unsupported "declaration"
    }


unsupported :: String -> String -> Q a
unsupported kind _ =
  fail ("jsonspec: " <> kind <> " contexts are not supported; use as a type")


quoteJsonSpecType :: String -> Q Type
quoteJsonSpecType input =
  case parseProgram "jsonspec" (T.pack input) of
    Left err ->
      fail err
    Right Program { programSpec = body } ->
      promotedT 'Module `appT` specType body


specType :: Spec -> TypeQ
specType StringSpec =
  promotedT 'JsonString
specType NumberSpec =
  promotedT 'JsonNum
specType IntSpec =
  promotedT 'JsonInt
specType BoolSpec =
  promotedT 'JsonBool
specType DateTimeSpec =
  promotedT 'JsonDateTime
specType RawSpec =
  promotedT 'JsonRaw
specType (TagSpec t) =
  promotedT 'JsonTag `appT` symbolType t
specType (RefSpec n) =
  promotedT 'JsonRef `appT` symbolType n
specType (DictSpec s) =
  promotedT 'JsonDict `appT` specType s
specType (NullSpec s) =
  promotedT 'JsonNullable `appT` specType s
specType (ArraySpec s) =
  promotedT 'JsonArray `appT` specType s
specType (EitherSpec ss) =
  promotedT 'JsonEither `appT` listType (fmap specType ss)
specType (ObjectSpec fields) =
  promotedT 'JsonObject `appT` listType (fmap fieldType fields)
specType (LetSpec binds body) =
  (promotedT 'JsonLet `appT` listType (fmap bindingType binds))
    `appT` specType body


fieldType :: Field -> TypeQ
fieldType Field { fieldName = name, fieldOptional = True, fieldSpec = s } =
  (promotedT 'Optional `appT` symbolType name) `appT` specType s
fieldType Field { fieldName = name, fieldOptional = False, fieldSpec = s } =
  (promotedT 'Required `appT` symbolType name) `appT` specType s


bindingType :: Binding -> TypeQ
bindingType (TypeBinding name s) =
  (promotedT 'TypeBind `appT` symbolType name) `appT` specType s
bindingType (ModuleBinding name s) =
  (promotedT 'ModuleBind `appT` symbolType name)
    `appT` (promotedT 'Module `appT` specType s)


listType :: [TypeQ] -> TypeQ
listType =
  foldr
    (\t acc -> promotedT '(:) `appT` t `appT` acc)
    (promotedT '[])


symbolType :: Text -> TypeQ
symbolType t =
  litT (strTyLit (T.unpack t))
