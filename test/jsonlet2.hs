{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.JsonSpec
  ( HasJsonEncodingSpec(EncodingSpec), Module(Module)
  , Specification(JsonInt, JsonLet, JsonModule, JsonObject, JsonRef, JsonString)
  , type (:::), type (:=)
  )
import Data.JsonSpec.Codec.Tuple
  ( Field(Field), Ref(Ref), TupleEncoding(toJsonStructure), encode
  )
import Data.Proxy (Proxy(Proxy))
import Prelude (IO, Int, print)

newtype Wrapper a = Wrapper a

instance
    HasJsonEncodingSpec (Wrapper a)
  where
    type EncodingSpec (Wrapper a) =
      'Module
        (JsonLet
        '[ "Unused" := JsonString ]
        (JsonObject '[ "payload" ::: JsonModule (EncodingSpec a)] ))

instance
    (TupleEncoding a)
  =>
    TupleEncoding (Wrapper a)
  where
    toJsonStructure (Wrapper w) = (Field @"payload" (toJsonStructure w), ())

newtype MyInt = MyInt Int
instance HasJsonEncodingSpec MyInt where
  type EncodingSpec MyInt  = 'Module (JsonInt)
instance TupleEncoding MyInt where
  toJsonStructure (MyInt i) = i


newtype MyInt2 = MyInt2 Int
instance HasJsonEncodingSpec MyInt2 where
  type EncodingSpec MyInt2 =
    'Module
      (JsonLet '[ "Int" := JsonInt ] (JsonRef "Int"))
instance TupleEncoding MyInt2 where
  toJsonStructure (MyInt2 i) = Ref i


main :: IO ()
main = do
  print
    (
      encode
        (Proxy @(EncodingSpec (Wrapper MyInt)))
        (toJsonStructure (Wrapper (MyInt 1)))
    )
  print
    (
      encode
        (Proxy @(EncodingSpec (Wrapper MyInt2)))
        (toJsonStructure (Wrapper (MyInt2 1)))
    )
