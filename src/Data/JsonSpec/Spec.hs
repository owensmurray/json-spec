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
  JSONStructure,
  sym,
  Tag(..),
  Field(..),
  unField,
  Ref(..),
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
import GHC.Records (HasField(getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified GHC.TypeError as GE
import Prelude (Either, Maybe(Just, Nothing), ($), Bool, Eq, Int, Show)


{-|
  Simple DSL for defining type level "specifications" for JSON
  data. Similar in spirit to (but not isomorphic with) JSON Schema.

  Intended to be used at the type level using @-XDataKinds@

  See 'JSONStructure' for how these map into Haskell representations.
-}
data Specification where
  JsonObject :: [FieldSpec] -> Specification
    {-^
      An object with the specified properties, each having its own
      specification. This does not yet support optional properties,
      although a property can be specified as "nullable" using
      `JsonNullable`
    -}
  JsonString :: Specification
    {-^ An arbitrary JSON string. -}
  JsonNum :: Specification
    {-^ An arbitrary (floating point) JSON number. -}
  JsonInt :: Specification
    {-^ A JSON integer.  -}
  JsonArray :: Specification -> Specification
    {-^ A JSON array of values which conform to the given spec. -}
  JsonBool :: Specification
    {-^ A JSON boolean value. -}
  JsonNullable :: Specification -> Specification
    {-^
      A value that can either be `null`, or else a value conforming to
      the specification.

      E.g.:

      > type SpecWithNullableField =
      >   JsonObject '[
      >     Required "nullableProperty" (JsonNullable JsonString)
      >   ]
    -}
  JsonEither :: [Specification] -> Specification
    {-^
      One of several different specifications. Corresponds to json-schema
      "oneOf". Useful for encoding sum types.

      Takes a type-level list of specs. In the structural representation
      ('JStruct'), `JsonEither` maps to nested `Either`: two or more
      branches become @Either (JStruct env a) (Either (JStruct env b) ...)@;
      a single branch maps to @JStruct env spec@ (no sum wrapper). Use
      `Left`/`Right` for construction and pattern matching.

      Example:

      > data MyType
      >   = Foo Text
      >   | Bar Int
      >   | Baz UTCTime
      > instance HasJsonEncodingSpec MyType where
      >   type EncodingSpec MyType =
      >     JsonEither
      >       '[
      >         JsonObject '[
      >           Required "tag" (JsonTag "foo"),
      >           Required "content" JsonString
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "bar"),
      >           Required "content" JsonInt
      >         ],
      >         JsonObject '[
      >           Required "tag" (JsonTag "baz"),
      >           Required "content" JsonDateTime
      >         ]
      >       ]
      >
      >   toJSONStructure = \case
      >     Foo t ->
      >       Left
      >         ( Field @"tag" (Tag @"foo")
      >         , (Field @"content" t, ())
      >         )
      >     Bar i ->
      >       Right (Left
      >         ( Field @"tag" (Tag @"bar")
      >         , (Field @"content" i, ())
      >         )
      >     Baz dt ->
      >       Right (Right
      >         ( Field @"tag" (Tag @"baz")
      >         , (Field @"content" dt, ())
      >         )
    -}
  JsonTag :: Symbol -> Specification
    {-^ A constant string value -}
  JsonDateTime :: Specification
    {-^
      A JSON string formatted as an ISO-8601 string. In Haskell this
      corresponds to `Data.Time.UTCTime`, and in json-schema it corresponds
      to the "date-time" format.
    -}
  JsonLet :: [(Symbol, Specification)] -> Specification -> Specification
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
      >        "vertex1" ::: JsonRef "Vertex",
      >        "vertex2" ::: JsonRef "Vertex",
      >        "vertex3" ::: JsonRef "Vertex"
      >      ])

      Another use is to define recursive types:

      > type LabelledTree =
      >   JsonLet
      >     '[
      >       '("LabelledTree", JsonObject '[
      >         "label" ::: JsonString,
      >         "children" ::: JsonArray (JsonRef "LabelledTree")
      >        ])
      >      ]
      >     (JsonRef "LabelledTree")
    -}
  JsonRef :: Symbol -> Specification
    {-^
      A reference to a specification which has been defined in a surrounding
      'JsonLet'.
    -}
  JsonRaw :: Specification
    {-^ Some raw, uninterpreted JSON value -}
  JsonAnnotated :: forall k. [(Symbol, k)] -> Specification -> Specification
    {-^
      An annotation on a specification. This is purely for documentation
      purposes and has no effect on encoding or decoding. The annotations
      are a list of key-value pairs at the type level. Keys are always
      symbols (type-level strings). Values can be any kind @k@: strings
      ('Symbol'), booleans ('Bool'), natural numbers ('Nat'), or any
      custom promoted type the user defines. Within one list, all values
      must have the same kind.

      E.g.:

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

  Note! "Object structures" of this type have the appropriate 'HasField'
  instances, which allows you to use -XOverloadedRecordDot to extract
  values as an alternative to pattern matching the whole tuple structure
  when building your 'HasJsonDecodingSpec' instances. See @TestHasField@
  in the tests for an example

  Arrays, booleans, numbers, and strings are just Lists, 'Bool's,
  'Scientific's, and 'Text's respectively.

  If the user can convert their normal business logic type to/from this
  tuple type, then they get a JSON encoding to/from their type that is
  guaranteed to be compliant with the 'Specification'
-}
type family JSONStructure (spec :: Specification) where
  JSONStructure spec = JStruct '[] spec


{-|
  Make the correct reference type by looking up the symbol, and providing
  the environment in which the symbol was _defined_. We mustn't use the
  environment in which the reference is _used_, or else 'Specification'
  would be a dynamically scoped language, instead of a statically scoped
  language.
-}
type family
    LookupRef
      (env :: Env)
      (search :: Env)
      (target :: Symbol)
    :: Type
  where
    LookupRef
        env
        ( ('(target, spec) : moreDefs) : moreStack )
        target
      =
        Ref env spec

    LookupRef
        env
        ( ('(miss, spec) : moreDefs) : moreStack)
        target
      =
        LookupRef env ( moreDefs : moreStack) target

    LookupRef
        env
        ( '[] : moreStack)
        target
      =
        LookupRef moreStack moreStack target


type family PushAll (a :: [k]) (b :: [k]) :: [k] where
  PushAll '[] b = b
  PushAll (e : more) b = PushAll more (e : b)


{-|
  Structural type for `JsonEither`: nested `Either` for two or more branches,
  or the lone branch type for a singleton list. Empty list is disallowed.
-}
type family EitherJStruct (env :: Env) (specs :: [Specification]) :: Type where
  EitherJStruct _env '[] =
    GE.TypeError (GE.Text "JsonEither requires at least one branch")
  EitherJStruct env '[spec] =
    JStruct env spec
  EitherJStruct env (a ': b ': more) =
    Either (JStruct env a) (EitherJStruct env (b ': more))


type family
  JStruct
    (env :: Env)
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
    JStruct env (JsonEither specs) =
      EitherJStruct env specs
    JStruct env (JsonTag tag) = Tag tag
    JStruct env JsonDateTime = UTCTime
    JStruct env (JsonNullable spec) = Maybe (JStruct env spec)
    JStruct env (JsonLet defs spec) =
      JStruct (defs : env) spec
    JStruct env (JsonRef ref) = LookupRef env env ref
    JStruct env JsonRaw = Value
    JStruct env (JsonAnnotated _annotations spec) =
      JStruct env spec


{-|
  This is the "Haskell structure" type of 'JsonRef' references.

  The main reason why we need this is because of recursion, as explained
  below:

  Since the specification is at the type level, and type level haskell
  is strict, specifying a recursive definition the "naive" way would
  cause an infinitely sized type.

  For example this won't work:

  > data Foo = Foo [Foo]
  > instance HasJsonEncodingSpec Foo where
  >   type EncodingSpec Foo = JsonArray (EncodingSpec Foo)
  >   toJSONStructure = ... can't be written

  ... because @EncodingSpec Foo@ would expand strictly into an array of
  @EncodingSpec Foo@, which would expand strictly... to infinity.

  Using `JsonLet` prevents the specification type from being infinitely
  sized, but what about the "structure" type which holds real values
  corresponding to the spec? The structure type has to have some way to
  reference itself or else it too would be infinitely sized.

  In order to "reference itself" the structure type has to go through
  a newtype somewhere along the way, and that's what this type is
  for. Whenever you use a 'JsonRef' in the spec, the corresponding
  structural type will have a 'Ref' newtype wrapper around the
  "dereferenced" structure type.

  For example:

  > data Foo = Foo [Foo]
  > instance HasJsonEncodingSpec Foo where
  >   type EncodingSpec Foo =
  >     JsonLet
  >       '[ '("Foo", JsonArray (JsonRef "Foo")) ]
  >       (JsonRef "Foo")
  >   toJSONStructure (Foo fs) =
  >     Ref [ toJSONStructure <$> fs ]

  Strictly speaking, we wouldn't /necessarily/ have to translate every
  'JsonRef' into a 'Ref'. In principal we could get away with inserting a
  'Ref' somewhere in every mutually recursive cycle. But the type level
  programming to figure that out a) probably wouldn't do any favors to
  compilation times, b) is beyond what I'm willing to attempted right
  now, and c) requires some kind of deterministic and stable choice
  about where to insert the 'Ref' (which I'm not even certain exists)
  lest arbitrary 'HasJsonEncodingSpec' or 'HasJsonDecodingSpec' instances
  break when the members of the recursive cycle change, causing a new
  choice about where to place the 'Ref'.
-}
newtype Ref env spec = Ref
  { unRef :: JStruct env spec
  }


{-| Structural representation of 'JsonTag'. (I.e. a constant string value.) -}
data Tag (a :: Symbol) = Tag


{-| Structural representation of an object field. -}
newtype Field (key :: Symbol) t = Field t
  deriving stock (Show, Eq)
instance {-# overlappable #-} (HasField k more v) => HasField k (Field notIt x, more) v where
  getField (_, more) = getField @k @_ @v more
instance {-# overlappable #-} (HasField k more v) => HasField k (Maybe (Field notIt x), more) v where
  getField (_, more) = getField @k @_ @v more
instance HasField k (Maybe (Field k v), more) (Maybe v) where
  getField (mv, _) =
    case mv of
      Nothing -> Nothing
      Just (Field v) -> Just v
instance HasField k (Field k v, more) v where
  getField (Field v, _) = v


unField :: Field key t -> t
unField (Field t) = t


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


type Env = [[(Symbol, Specification)]]

