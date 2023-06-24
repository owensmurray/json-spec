{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.JsonSpec.Spec (
  Specification(..),
  JSONStructure,
  sym,
  Tag(..),
  Field(..),
) where


import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


{-|
  Simple DSL for defining type level "specifications" for JSON
  data. Similar in spirit to (but not isomorphic with) JSON Schema.
  
  Intended to be used at the type level using @-XDataKinds@

  See 'JSONStructure' for how these map into Haskell representations.
-}
data Specification
  = JsonObject [(Symbol, Specification)]
    {-^
      An object with the specified properties, each having its own
      specification. This does not yet support optional properties,
      although a property can be specified as "nullable" using
      `JsonNullable`
    -}
  | JsonString
    {-^ An arbitrary JSON string. -}
  | JsonNum
    {-^ An arbitrary (floating point) JSON number. -}
  | JsonInt
    {-^ A JSON integer.  -}
  | JsonArray Specification
    {-^ A JSON array of values which conform to the given spec. -}
  | JsonBool
    {-^ A JSON boolean value. -}
  | JsonNullable Specification
    {-^
      A value that can either be `null`, or else a value conforming to
      the specification.

      E.g.:

      > type SpecWithNullableField =
      >   JsonObject
      >     '[ '("nullableProperty", JsonNullable JsonString)
      >      ]
    -}
  | JsonEither Specification Specification
    {-^
      One of two different specifications. Corresponds to json-schema
      "oneOf". Useful for encoding sum types. E.g:

      > data MyType
      >   = Foo Text
      >   | Bar Int
      >   | Baz UTCTime
      > instance HasJsonEncodingSpec MyType where
      >   type EncodingSpec MyType =
      >     JsonEither
      >       (
      >         JsonObject
      >           '[ '("tag", JsonTag "foo")
      >            , '("content", JsonString)
      >            ]
      >       )
      >       (
      >         JsonEither
      >           (
      >             JsonObject
      >               '[ '("tag", JsonTag "bar")
      >                , '("content", JsonInt)
      >                ]
      >           )
      >           (
      >             JsonObject
      >               '[ '("tag", JsonTag "baz")
      >                , '("content", JsonDateTime)
      >                ]
      >           )
      >       )
    -}
  | JsonTag Symbol {-^ A constant string value -}
  | JsonDateTime
    {-^
      A JSON string formatted as an ISO-8601 string. In Haskell this
      corresponds to `Data.Time.UTCTime`, and in json-schema it corresponds
      to the "date-time" format.
    -}


{- |
  @'JSONStructure' spec@ is the Haskell type used to contain the JSON data
  that will be encoded or decoded according to the provided @spec@.

  Basically, we represent JSON objects as "list-like" nested tuples of
  the form:

  > (Field @key1 valueType,
  > (Field @key2 valueType,
  > (Field @key3 valueType,
  > ())))

  Arrays, booleans, numbers, and strings are just Lists, 'Bool's,
  'Scientific's, and 'Text's respectively.

  If the user can convert their normal business logic type to/from this
  tuple type, then they get a JSON encoding to/from their type that is
  guaranteed to be compliant with the 'Specification'
-}
type family JSONStructure (spec :: Specification) where
  JSONStructure (JsonObject '[]) = ()
  JSONStructure (JsonObject ( '(key, s) : more )) =
    (
      Field key (JSONStructure s),
      JSONStructure (JsonObject more)
    )
  JSONStructure JsonString = Text
  JSONStructure JsonNum = Scientific
  JSONStructure JsonInt = Int
  JSONStructure (JsonArray spec) = [JSONStructure spec]
  JSONStructure JsonBool = Bool
  JSONStructure (JsonEither left right) =
    Either (JSONStructure left) (JSONStructure right)
  JSONStructure (JsonTag tag) = Tag tag
  JSONStructure JsonDateTime = UTCTime
  JSONStructure (JsonNullable spec) = Maybe (JSONStructure spec)


{-| Structural representation of 'JsonTag'. (I.e. a constant string value.) -}
data Tag (a :: Symbol) = Tag


{-| Structural representation of an object field. -}
newtype Field (key :: Symbol) t = Field t


{- |
  Shorthand for demoting type-level strings.
  Use with -XTypeApplication, e.g.:

  This function doesn't really "go" in this module, it is only here because
  this module happens to be at the bottom of the dependency tree and so it is
  easy to stuff "reusable" things here, and I don't feel like creating a whole
  new module just for this function (although maybe I should).

  > sym @var
-}
sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


