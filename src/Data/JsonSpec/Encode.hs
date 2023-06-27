{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Encode (
  HasJsonEncodingSpec(..),
  StructureToJSON(..),
) where


import Data.Aeson (ToJSON(toJSON), Value)
import Data.JsonSpec.Spec (Field(Field), JSONStructure, Specification,
  Tag, sym)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM


{- |
  Types of this class can be encoded to JSON according to a type-level
  'Specification'.
-}
class HasJsonEncodingSpec a where
  {- | The encoding specification. -}
  type EncodingSpec a :: Specification

  {- | Encode the value into the structure appropriate for the specification. -}
  toJSONStructure :: a -> JSONStructure (EncodingSpec a)


{- |
  This is like 'ToJSON', but specialized for our custom "json
  representation" types (i.e. the 'JSONStructure' type family). It is
  also closed (i.e. not exported, so the user can't add instances),
  because our json representation is closed.

  see 'StructureFromJSON' for an explaination about why we don't just use
  'ToJSON'.
-}
class StructureToJSON a where
  reprToJSON :: a -> Value
instance StructureToJSON () where
  reprToJSON () = A.object []
instance StructureToJSON Text where
  reprToJSON = toJSON
instance StructureToJSON Scientific where
  reprToJSON = toJSON
instance StructureToJSON Int where
  reprToJSON = toJSON
instance (ToJSONObject (a, b)) => StructureToJSON (a, b) where
  reprToJSON = A.Object . toJSONObject 
instance (StructureToJSON left, StructureToJSON right) => StructureToJSON (Either left right) where
  reprToJSON = \case
    Left val -> reprToJSON val
    Right val -> reprToJSON val
instance (KnownSymbol const) => StructureToJSON (Tag const) where
  reprToJSON _proxy = toJSON (sym @const @Text)
instance (StructureToJSON a) => StructureToJSON [a] where
  reprToJSON = toJSON . fmap reprToJSON
instance StructureToJSON UTCTime where
  reprToJSON = toJSON
instance (StructureToJSON a) => StructureToJSON (Maybe a) where
  reprToJSON = maybe A.Null reprToJSON


{- |
  This class is to help 'StructureToJSON' recursively encode objects, and
  is mutually recursive with 'StructureToJSON'. If we tried to "recurse
  on the rest of the object" directly in 'StructureToJSON' we would end
  up with a partial function, because 'reprToJSON' returns a 'Value'
  not an 'Object'. We would therefore have to pattern match on 'Value'
  to get the 'Object' back out, but we would have to call 'error' if the
  'Value' mysteriously somehow wasn't an 'Object' after all. Instead of
  calling error because "it can't ever happen", we use this helper so
  the compiler can prove it never happens.
-}
class ToJSONObject a where
  toJSONObject :: a -> A.Object
instance ToJSONObject () where
  toJSONObject _ = mempty
instance (KnownSymbol key, StructureToJSON val, ToJSONObject more) => ToJSONObject (Field key val, more) where
  toJSONObject (Field val, more) =
    KM.insert
      (sym @key)
      (reprToJSON val)
      (toJSONObject more)


