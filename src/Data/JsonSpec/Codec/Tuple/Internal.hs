{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Internal tuple-structure representation. Not part of the public API. -}
module Data.JsonSpec.Codec.Tuple.Internal (
  JsonStructure,
  JStruct,
  Tag(..),
  Field(..),
  unField,
  Ref(..),
  sym,
) where

import Data.Aeson (Value)
import Data.JsonSpec.Spec
  ( BindingSpec(ModuleBind, TypeBind), FieldSpec(Optional, Required)
  , Module(Module)
  , Specification
    ( JsonAnnotated, JsonArray, JsonBool, JsonDateTime, JsonDict, JsonEither
    , JsonInt, JsonLet, JsonModule, JsonNullable, JsonNum, JsonObject, JsonRaw
    , JsonRef, JsonString, JsonTag
    )
  )
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Records (HasField(getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude (Maybe(Just, Nothing), ($), Bool, Either, Eq, Int, Show)
import qualified GHC.TypeError as GE

{- |
  @'JsonStructure' spec@ is the Haskell type used to contain the JSON data
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

  Arrays, dicts, booleans, numbers, and strings are just Lists,
  @'Map' 'Text'@, 'Bool's, 'Scientific's, and 'Text's respectively.

  If the user can convert their normal business logic type to/from this
  tuple type, then they get a JSON encoding to/from their type that is
  guaranteed to be compliant with the 'Specification'
-}
type family JsonStructure (spec :: Module) where
  JsonStructure ('Module s) = JStruct '[] s


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
    JStruct env (JsonDict spec) = Map Text (JStruct env spec)
    JStruct env JsonBool = Bool
    JStruct env (JsonEither specs) =
      EitherJStruct env specs
    JStruct env (JsonTag tag) = Tag tag
    JStruct env JsonDateTime = UTCTime
    JStruct env (JsonNullable spec) = Maybe (JStruct env spec)
    JStruct env (JsonLet defs spec) =
      JStruct (BindingsToFrame defs : env) spec
    JStruct env (JsonRef ref) = LookupRef env env ref
    JStruct env (JsonModule m) =
      JsonStructure m
    JStruct env JsonRaw = Value
    JStruct env (JsonAnnotated _annotations spec) =
      JStruct env spec


{-| Lower 'BindingSpec's to the env-frame representation. -}
type family BindingsToFrame (bs :: [BindingSpec]) :: [(Symbol, Specification)] where
  BindingsToFrame '[] = '[]
  BindingsToFrame (TypeBind n s : more) =
    '(n, s) : BindingsToFrame more
  BindingsToFrame (ModuleBind n s : more) =
    '(n, JsonModule s) : BindingsToFrame more


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
  >   toJsonStructure = ... can't be written

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
  >       '[ "Foo" := JsonArray (JsonRef "Foo") ]
  >       (JsonRef "Foo")
  >   toJsonStructure (Foo fs) =
  >     Ref [ toJsonStructure <$> fs ]

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
