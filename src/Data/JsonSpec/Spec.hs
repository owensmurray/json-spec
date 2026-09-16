{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Spec (
  Specification(..),
  Module(..),
  BindingSpec(..),
  FieldSpec(..),
  (:::),
  (::?),
  (:=),
  (::=),
  HasJsonEncodingSpec(..),
  HasJsonDecodingSpec(..),
) where

import GHC.TypeLits (Symbol)
import Prelude ()

{-|
  Type-level AST for JSON structure specifications.

  Use with @-XDataKinds@. Codecs such as 'Data.JsonSpec.Codec.Tuple'
  interpret these specs into concrete Haskell types and
  encode/decode strategies.

  Similar in spirit to JSON Schema, but not isomorphic with it.
  The matching textual language is documented in
  @docs\/language-spec.md@.
-}
data Specification where
  JsonObject :: [FieldSpec] -> Specification
    {-^
      Object with a fixed set of fields. Use 'Required' / 'Optional'
      (or '(:::)' / '(::?)') for each field.
    -}
  JsonString :: Specification
    {-^ Any JSON string. -}
  JsonNum :: Specification
    {-^ Any JSON number (floating point). -}
  JsonInt :: Specification
    {-^ A JSON integer. -}
  JsonArray :: Specification -> Specification
    {-^ Array whose elements all conform to the given spec. -}
  JsonDict :: Specification -> Specification
    {-^
      Object used as a string-keyed map: keys are unrestricted, and
      every value must conform to the given spec.

      Distinct from 'JsonObject', which has statically known field
      names.
    -}
  JsonBool :: Specification
    {-^ A JSON boolean. -}
  JsonNullable :: Specification -> Specification
    {-^
      Either JSON @null@, or a value conforming to the given spec.

      > type SpecWithNullableField =
      >   JsonObject '[
      >     Required "nullableProperty" (JsonNullable JsonString)
      >   ]
    -}
  JsonEither :: [Specification] -> Specification
    {-^
      Exactly one of the given alternatives (json-schema @oneOf@).
      Commonly used for sum types.

      > data MyType
      >   = Foo Text
      >   | Bar Int
      >   | Baz UTCTime
      > instance HasJsonEncodingSpec MyType where
      >   type EncodingSpec MyType =
      >     'Module
      >       (JsonEither
      >         '[
      >           JsonObject '[
      >             Required "tag" (JsonTag "foo"),
      >             Required "content" JsonString
      >           ],
      >           JsonObject '[
      >             Required "tag" (JsonTag "bar"),
      >             Required "content" JsonInt
      >           ],
      >           JsonObject '[
      >             Required "tag" (JsonTag "baz"),
      >             Required "content" JsonDateTime
      >           ]
      >         ])
    -}
  JsonTag :: Symbol -> Specification
    {-^ A constant string value. -}
  JsonDateTime :: Specification
    {-^
      ISO-8601 date-time string. Maps to 'Data.Time.UTCTime' in
      Haskell and to the json-schema @"date-time"@ format.
    -}
  JsonLet :: [BindingSpec] -> Specification -> Specification
    {-^
      Bind names, then use them in the body via 'JsonRef'.

      'TypeBind' is open: the RHS can refer to sibling bindings and
      outer lets. 'ModuleBind' is closed: the RHS is a 'Module' and
      cannot see outer names.

      Bindings in the same let may refer to each other, including
      recursively.

      > type Triangle =
      >   JsonLet
      >     '[
      >       "Vertex" := JsonObject '[
      >         "x" ::: JsonInt,
      >         "y" ::: JsonInt,
      >         "z" ::: JsonInt
      >       ]
      >     ]
      >     (JsonObject '[
      >       "vertex1" ::: JsonRef "Vertex",
      >       "vertex2" ::: JsonRef "Vertex",
      >       "vertex3" ::: JsonRef "Vertex"
      >     ])

      Recursive:

      > type LabelledTree =
      >   JsonLet
      >     '[
      >       "LabelledTree" := JsonObject '[
      >         "label" ::: JsonString,
      >         "children" ::: JsonArray (JsonRef "LabelledTree")
      >       ]
      >     ]
      >     (JsonRef "LabelledTree")

      Closed nested binding ('ModuleBind' / '(::=)'):

      > type Invoice =
      >   JsonLet
      >     '[
      >       "Id" := JsonString,
      >       "Tax" ::=
      >         'Module
      >           (JsonLet
      >             '[ "Rate" := JsonNum ]
      >             (JsonObject '[ "rate" ::: JsonRef "Rate" ]))
      >     ]
      >     (JsonObject '[
      >       "id" ::: JsonRef "Id",
      >       "tax" ::: JsonRef "Tax"
      >     ])
    -}
  JsonRef :: Symbol -> Specification
    {-^
      Reference a name bound by an enclosing 'JsonLet'.

      Resolution uses the environment from the binding site, not
      from the reference site.
    -}
  JsonModule :: Module -> Specification
    {-^
      Embed a closed 'Module' inside another specification. The
      embedded module cannot see names from any outer 'JsonLet'.

      Typical use: nest another type's 'EncodingSpec' (itself a
      'Module') without exposing the outer environment to it.

      > type EncodingSpec (Wrapper a) =
      >   'Module
      >     (JsonLet
      >       '[ "Unused" := JsonString ]
      >       (JsonObject '[
      >         "payload" ::: JsonModule (EncodingSpec a)
      >       ]))
    -}
  JsonRaw :: Specification
    {-^ An opaque JSON value; not further interpreted. -}
  JsonAnnotated :: forall k. [(Symbol, k)] -> Specification -> Specification
    {-^
      Attach documentation metadata to a specification. Has no effect
      on encoding or decoding.

      Annotations are type-level key-value pairs. Keys are always
      'Symbol'. Values share a single kind @k@ within one list —
      commonly 'Symbol', 'Bool', 'Nat', or a user-defined promoted
      type.

      > type AnnotatedUser =
      >   JsonAnnotated
      >     '[ '("description", "A user record")
      >      , '("example", "...")
      >      ]
      >     (JsonObject '[
      >       Required "name" JsonString,
      >       Optional "last-login" JsonDateTime
      >      ])
      >
      > type ReadOnlyObject =
      >   JsonAnnotated '[ '("readOnly", 'True) ] (JsonObject '[])
    -}


{-|
  A closed specification: no free references to an outer
  environment.

  Corresponds to @module@ in the textual language. Also the return
  kind of 'EncodingSpec' / 'DecodingSpec', so associated codecs are
  closed by construction.
-}
data Module = Module Specification


{-|
  A named binding in a 'JsonLet'.

  'TypeBind' is open; 'ModuleBind' is closed. Neither introduces a
  namespace — there is no @M.N@ path syntax.
-}
data BindingSpec
  = TypeBind Symbol Specification
    {-^
      Open binding (@type Name = …@). May refer to siblings in this
      let and to names from outer lets.
    -}
  | ModuleBind Symbol Module
    {-^
      Closed binding (@module Name = …@). The RHS is a 'Module' and
      cannot see outer names. Useful with 'EncodingSpec':

      > "Item" ::= EncodingSpec LineItem
    -}


{-| A field in a 'JsonObject'. -}
data FieldSpec
  = Required Symbol Specification {-^ Required field. -}
  | Optional Symbol Specification {-^ Optional field. -}


{-| Alias for 'Required'. -}
type (:::) = Required


{-| Alias for 'Optional'. -}
type (::?) = Optional


{-| Alias for 'TypeBind'. -}
type (:=) = TypeBind


{-| Alias for 'ModuleBind'. -}
type (::=) = ModuleBind


{-|
  Types that provide a closed encoding 'Module'.

  Closed means the specification is self-contained: it cannot
  reference names from any outer 'JsonLet'. That is why the
  associated type has kind 'Module' rather than 'Specification'.
-}
class HasJsonEncodingSpec a where
  {-|
    The encoding specification.

    Kind 'Module' enforces closedness: no free references to an
    outer environment.
  -}
  type EncodingSpec a :: Module


{-|
  Types that provide a closed decoding 'Module'.

  Closed means the specification is self-contained: it cannot
  reference names from any outer 'JsonLet'. That is why the
  associated type has kind 'Module' rather than 'Specification'.
-}
class HasJsonDecodingSpec a where
  {-|
    The decoding specification.

    Kind 'Module' enforces closedness: no free references to an
    outer environment.
  -}
  type DecodingSpec a :: Module
