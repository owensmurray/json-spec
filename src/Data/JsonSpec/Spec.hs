{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Spec (
  Specification(..),
  JSONStructure,
  sym,
  Tag(..),
  Field(..),
  Rec(..),
  JStruct,
  FieldSpec(..),
  (:::),
  (::?),
) where


import Data.Aeson (Value)
import Data.Kind (Type)
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
  = JsonObject [FieldSpec]
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
      >   JsonObject '[
      >     Required "nullableProperty" (JsonNullable JsonString)
      >   ]
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
      >         JsonObject '[
      >           Required "tag" (JsonTag "foo"),
      >           Required "content" JsonString
      >         ]
      >       )
      >       (
      >         JsonEither
      >           (
      >             JsonObject '[
      >               Required "tag" (JsonTag "bar"),
      >               Required "content" JsonInt
      >             ]
      >           )
      >           (
      >             JsonObject '[
      >               Required "tag" (JsonTag "baz"),
      >               Required "content" JsonDateTime
      >             ]
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
  | JsonLet [(Symbol, Specification)] Specification
    {-^
      A "let" expression. This is useful for giving names to types, which can
      then be used in the generated code.

      This is also useful to shorten repetitive type definitions. For example,
      this repetitive definition:

      > type Triangle =
      >   JsonObject '[
      >     Required "vertex1" (JsonObject '[
      >       Required "x" JsonInt,
      >       Required "y" JsonInt,
      >       Required "z" JsonInt
      >     ]),
      >     Required "vertex2" (JsonObject '[
      >       Required "x" JsonInt,
      >       Required "y" JsonInt,
      >       Required "z" JsonInt
      >     ]),
      >     Required "vertex3" (JsonObject '[
      >       Required "x" JsonInt),
      >       Required "y" JsonInt),
      >       Required "z" JsonInt)
      >     ])
      >   ]

      Can be written more concisely as:

      > type Triangle =
      >   JsonLet
      >     '[
      >       '("Vertex", JsonObject '[
      >          ('x', JsonInt),
      >          ('y', JsonInt),
      >          ('z', JsonInt)
      >        ])
      >      ]
      >      (JsonObject '[
      >        Required "vertex1" JsonRef "Vertex",
      >        Required "vertex2" JsonRef "Vertex",
      >        Required "vertex3" JsonRef "Vertex"
      >      ])

      Another use is to define recursive types:

      > type LabelledTree =
      >   JsonLet
      >     '[
      >       '("LabelledTree", JsonObject '[
      >         Required "label", JsonString,
      >         Required "children" (JsonArray (JsonRef "LabelledTree"))
      >        ])
      >      ]
      >     (JsonRef "LabelledTree")
    -}
  | JsonRef Symbol
    {-^
      A reference to a specification which has been defined in a surrounding
      'JsonLet'.
    -}
  | JsonRaw {-^ Some raw, uninterpreted JSON value -}



{-| Specify a field in an object.  -}
data FieldSpec
  = Required Symbol Specification {-^ The field is required -}
  | Optional Symbol Specification {-^ The field is optionsl -}


{-| Alias for 'Required'. -}
type (:::) = Required


{-| Alias for 'Optional'. -}
type (::?) = Optional


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
  JSONStructure spec = JStruct '[] spec


type family
  Append
    (defs :: [(Symbol, Specification)])
    (env :: [(Symbol, Type)])
  :: [(Symbol, Type)]
  where
    Append '[] env = env
    Append ( '(name, spec) : defs ) env =
      '( name
       , JStruct
           ( '(name, Rec env name spec) : env)
           spec
       )
      : Append defs env


type family
  Lookup
    (key :: Symbol)
    (env :: [(Symbol, Type)])
  :: Type
  where
    Lookup key ( '(key, spec) : more ) = spec
    Lookup key ( _ : more ) = Lookup key more


type family
  JStruct
    (env :: [(Symbol, Type)])
    (spec :: Specification)
  :: Type
  where
    JStruct env (JsonObject '[]) = ()
    JStruct env (JsonObject ( Required key s : more )) =
      (
        Field key (JStruct env s),
        JStruct env (JsonObject more)
      )
    JStruct env (JsonObject ( Optional key s : more )) =
      (
        Maybe (Field key (JStruct env s)),
        JStruct env (JsonObject more)
      )
    JStruct env JsonString = Text
    JStruct env JsonNum = Scientific
    JStruct env JsonInt = Int
    JStruct env (JsonArray spec) = [JStruct env spec]
    JStruct env JsonBool = Bool
    JStruct env (JsonEither left right) =
      Either (JStruct env left) (JStruct env right)
    JStruct env (JsonTag tag) = Tag tag
    JStruct env JsonDateTime = UTCTime
    JStruct env (JsonNullable spec) = Maybe (JStruct env spec)
    JStruct env (JsonLet defs spec) =
      JStruct (Append defs env) spec
    JStruct env (JsonRef ref) = Lookup ref env
    JStruct env JsonRaw = Value


{-|
  This allows for recursive specifications.

  Since the specification is at the
  type level, and type level haskell is strict, specifying a recursive
  definition the "naive" way would cause an infinitely sized type.

  For example this won't work:

  > data Foo = Foo [Foo]
  > instance HasJsonEncodingSpec Foo where
  >   type EncodingSpec Foo = JsonArray (EncodingSpec Foo)
  >   toJSONStructure = ... can't be written

  Using `JsonLet` prevents the specification type from being infinitely
  sized, but what about "structure" type which holds real values
  corresponding to the spec? The structure type has to have some way to
  reference itself or else it too would be infinitely sized.

  In order to "reference itself" the structure type has to go
  through a newtype somewhere along the way, and that's what this
  type is for. Whenever the structure type for your spec requires a
  self-reference, it will require you to wrap the recursed upon values
  in this type.

  For example:

  > data Foo = Foo [Foo]
  > instance HasJsonEncodingSpec Foo where
  >   type EncodingSpec Foo =
  >     JsonLet
  >       '[ '("Foo", JsonArray (JsonRef "Foo")) ]
  >       (JsonRef "Foo")
  >   toJSONStructure (Foo fs) =
  >     [ Rec (toJSONStructure f)
  >     | f <- fs
  >     ]
-}
newtype Rec env name spec = Rec
  { unRec ::
      JStruct
        ( '(name, Rec env name spec) : env)
        spec
  }



{-| Structural representation of 'JsonTag'. (I.e. a constant string value.) -}
data Tag (a :: Symbol) = Tag


{-| Structural representation of an object field. -}
newtype Field (key :: Symbol) t = Field {unField :: t}
  deriving stock (Show, Eq)


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


