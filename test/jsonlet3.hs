{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.JsonSpec
  ( HasJsonEncodingSpec(EncodingSpec), Module(Module)
  , Specification(JsonArray, JsonInt, JsonLet, JsonModule, JsonObject, JsonRef, JsonString)
  , type (:::), type (:=)
  )
import Data.JsonSpec.Codec.Tuple
  ( Field(Field), Ref(Ref), TupleEncoding(toJsonStructure), encode
  )
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, empty)
import Prelude ((.), (<$>), IO, Int, print)

-- A container polymorphic in its payload spec. Same shape as the library's
-- own Set instance. Compiles.
data LineItem a = LineItem
  { description :: Text
  , quantity :: Int
  , unitPrice :: a
  , lineTotal :: a
  }
instance HasJsonEncodingSpec (LineItem a) where
  type EncodingSpec (LineItem a) =
    'Module
      (JsonObject
      '[ "description" ::: JsonString
       , "quantity" ::: JsonInt
       , "unitPrice" ::: JsonModule (EncodingSpec a)
       , "lineTotal" ::: JsonModule (EncodingSpec a)
       ])
instance (TupleEncoding a) => TupleEncoding (LineItem a) where
  toJsonStructure li =
    ( Field li.description
    , ( Field li.quantity
      , ( Field (toJsonStructure li.unitPrice)
        , ( Field (toJsonStructure li.lineTotal)
          , ()))))

-- Name the LineItem spec in a JsonLet so we can share it within the invoice
-- spec, exactly as the Triangle/Vertex test names Vertex.
-- This will not compile.
data Invoice a = Invoice
  { invoiceNumber :: Text
  , items :: [LineItem a]
  }
instance HasJsonEncodingSpec (Invoice a) where
  type EncodingSpec (Invoice a) =
    'Module
      (JsonLet
      '[ "LineItem" := JsonModule (EncodingSpec (LineItem a)) ]
      (JsonObject
        '[ "invoiceNumber" ::: JsonString
         , "items" ::: JsonArray (JsonRef "LineItem")
         ]))
instance (TupleEncoding a) => TupleEncoding (Invoice a) where
  toJsonStructure inv =
    ( Field inv.invoiceNumber
    , ( Field (Ref . toJsonStructure <$> inv.items)
      , ()))


newtype Money = Money Int
instance HasJsonEncodingSpec Money where
  type EncodingSpec Money  = 'Module (JsonInt)
instance TupleEncoding Money where
  toJsonStructure (Money i) = i


main :: IO ()
main =
  let
    lineItem :: LineItem Money
    lineItem =
      LineItem
        { description = empty
        , quantity = 0
        , unitPrice = Money 0
        , lineTotal = Money 0
        }

    invoice :: Invoice Money
    invoice =
      Invoice
        { invoiceNumber = empty
        , items = [lineItem]
        }
  in
    print
      (encode
        (Proxy @(EncodingSpec (Invoice Money)))
        (toJsonStructure invoice))
