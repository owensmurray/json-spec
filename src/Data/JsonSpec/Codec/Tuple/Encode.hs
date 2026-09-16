{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Codec.Tuple.Encode (
  TupleEncoding(..),
  StructureToJson(..),
  encode,
) where

import Data.Aeson (ToJSON(toJSON), Value)
import Data.JsonSpec.Codec.Tuple.Internal
  ( Field(Field), Ref(unRef), JStruct, JsonStructure, Tag, sym
  )
import Data.JsonSpec.Spec (HasJsonEncodingSpec(EncodingSpec))
import Data.Map (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Either(Left, Right), Functor(fmap), Maybe(Just, Nothing), Monoid(mempty)
  , (.), Bool, Int, id, maybe
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as Map

{- |
  Encode a value into the structure appropriate for its specification.
-}
class (HasJsonEncodingSpec a) => TupleEncoding a where
  {- | Encode the value into the structure appropriate for the specification. -}
  toJsonStructure :: a -> JsonStructure (EncodingSpec a)


{- |
  This is like 'ToJSON', but specialized for our custom "json
  representation" types (i.e. the 'JsonStructure' type family). It is
  also closed (i.e. not exported, so the user can't add instances),
  because our json representation is closed.

  see 'StructureFromJson' for an explaination about why we don't just use
  'ToJSON'.
-}
class StructureToJson a where
  reprToJson :: a -> Value
instance StructureToJson Value where
  reprToJson = id
instance StructureToJson () where
  reprToJson () = A.object []
instance StructureToJson Bool where
  reprToJson = toJSON
instance StructureToJson Text where
  reprToJson = toJSON
instance StructureToJson Scientific where
  reprToJson = toJSON
instance StructureToJson Int where
  reprToJson = toJSON
instance (ToJsonObject (a, b)) => StructureToJson (a, b) where
  reprToJson = A.Object . toJsonObject
instance (StructureToJson left, StructureToJson right) => StructureToJson (Either left right) where
  reprToJson = \case
    Left val -> reprToJson val
    Right val -> reprToJson val
instance (KnownSymbol const) => StructureToJson (Tag const) where
  reprToJson _proxy = toJSON (sym @const @Text)
instance (StructureToJson a) => StructureToJson [a] where
  reprToJson = toJSON . fmap reprToJson
instance (StructureToJson a) => StructureToJson (Map Text a) where
  reprToJson =
    A.Object
      . KM.fromList
      . fmap (\(key, val) -> (AK.fromText key, reprToJson val))
      . Map.toList
instance StructureToJson UTCTime where
  reprToJson = toJSON
instance (StructureToJson a) => StructureToJson (Maybe a) where
  reprToJson = maybe A.Null reprToJson
instance
    (StructureToJson (JStruct env spec))
  =>
    StructureToJson (Ref env spec)
  where
    reprToJson = reprToJson . unRef


{- |
  This class is to help 'StructureToJson' recursively encode objects, and
  is mutually recursive with 'StructureToJson'. If we tried to "recurse
  on the rest of the object" directly in 'StructureToJson' we would end
  up with a partial function, because 'reprToJson' returns a 'Value'
  not an 'Object'. We would therefore have to pattern match on 'Value'
  to get the 'Object' back out, but we would have to call 'error' if the
  'Value' mysteriously somehow wasn't an 'Object' after all. Instead of
  calling error because "it can't ever happen", we use this helper so
  the compiler can prove it never happens.
-}
class ToJsonObject a where
  toJsonObject :: a -> A.Object
instance ToJsonObject () where
  toJsonObject _ = mempty
instance (KnownSymbol key, StructureToJson val, ToJsonObject more) => ToJsonObject (Field key val, more) where
  toJsonObject (Field val, more) =
    KM.insert
      (sym @key)
      (reprToJson val)
      (toJsonObject more)
instance (KnownSymbol key, StructureToJson val, ToJsonObject more) => ToJsonObject (Maybe (Field key val), more) where
  toJsonObject (mval, more) =
    case mval of
      Nothing -> toJsonObject more
      Just (Field val) ->
        KM.insert
          (sym @key)
          (reprToJson val)
          (toJsonObject more)


{-|
  Given a raw Haskell structure, directly encode it directly into an
  aeson Value without having to go through any To/FromJSON instances.

  See also: `Data.JsonSpec.Codec.Tuple.eitherDecode`.
-}
encode :: StructureToJson (JsonStructure spec) => Proxy spec -> JsonStructure spec -> Value
encode Proxy = reprToJson
