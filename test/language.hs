{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Werror=missing-import-lists #-}

module Main (main) where

import Data.Either (isLeft)
import Data.JsonSpec
  ( BindingSpec(ModuleBind, TypeBind), FieldSpec(Optional, Required)
  , Module(Module)
  , Specification
    ( JsonArray, JsonBool, JsonDateTime, JsonDict, JsonEither, JsonInt, JsonLet
    , JsonNullable, JsonNum, JsonObject, JsonRaw, JsonRef, JsonString, JsonTag
    )
  )
import Data.JsonSpec.Language.Parser
  ( Binding(ModuleBinding, TypeBinding), Field(Field), Program(Program)
  , Spec
    ( ArraySpec, BoolSpec, DateTimeSpec, DictSpec, EitherSpec, IntSpec, LetSpec
    , NullSpec, NumberSpec, ObjectSpec, RawSpec, RefSpec, StringSpec, TagSpec
    )
  , parseProgram
  )
import Data.JsonSpec.Language.QQ (jsonspec)
import Data.Text (Text)
import Prelude (Applicative(pure), Bool(False, True), Either(Right), ($), IO)
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)
import qualified Test.Hspec as Hspec

{-| Proxy for types of kind 'Module'. -}
data Mod (m :: Module) = Mod


main :: IO ()
main =
  hspec suite


suite :: Hspec.Spec
suite = do
  describe "parser" parserTests
  describe "quasi-quoter" qqTests


parserTests :: Hspec.Spec
parserTests = do
  it "parses a trivial module" $
    parseProgram "trivial" "module Id = string"
      `shouldBe`
        Right (Program "Id" StringSpec)

  it "parses Person from the language spec" $
    parseProgram "Person" personSrc
      `shouldBe`
        Right (Program "Person" personAst)

  it "parses Graphs mutual recursion" $
    parseProgram "Graphs" graphsSrc
      `shouldBe`
        Right (Program "Graphs" graphsAst)

  it "parses Demo with nested closed module" $
    parseProgram "Demo" demoSrc
      `shouldBe`
        Right (Program "Demo" demoAst)

  it "parses either, dict, null, tags, and primitives" $
    parseProgram "Kitchen" kitchenSrc
      `shouldBe`
        Right (Program "Kitchen" kitchenAst)

  it "allows comments and trailing commas" $
    parseProgram "Comments" commentsSrc
      `shouldBe`
        Right
          (Program "Comments"
            (ObjectSpec
              [ Field "a" False IntSpec
              , Field "b" True StringSpec
              ]))

  it "rejects duplicate field names" $
    parseProgram "DupField"
      "module X = { \"a\": int, \"a\": string }"
      `shouldSatisfy` isLeft

  it "rejects duplicate binding names" $
    parseProgram "DupBind"
      "module X = let { type A = int type A = string in A }"
      `shouldSatisfy` isLeft

  it "rejects keyword used as bare identifier" $
    parseProgram "Keyword"
      "module type = string"
      `shouldSatisfy` isLeft

  it "allows backtick-escaped keyword as module name" $
    parseProgram "EscapedMod"
      "module `type` = string"
      `shouldBe`
        Right (Program "type" StringSpec)

  it "allows backtick-escaped keyword bind and ref" $
    parseProgram "EscapedBind"
      "module X = let { type `string` = int in `string` }"
      `shouldBe`
        Right
          (Program "X"
            (LetSpec
              [TypeBinding "string" IntSpec]
              (RefSpec "string")))

  it "keeps bare string as the primitive, not a ref" $
    parseProgram "BarePrim"
      "module X = let { type `string` = int in string }"
      `shouldBe`
        Right
          (Program "X"
            (LetSpec
              [TypeBinding "string" IntSpec]
              StringSpec))


qqTests :: Hspec.Spec
qqTests = do
  it "quotes a trivial module" $
    sameModule
      (Mod @TrivialQuoted)
      (Mod @('Module 'JsonString))

  it "quotes Person" $
    sameModule
      (Mod @PersonQuoted)
      (Mod @PersonExpected)

  it "quotes the exhaustive Demo program" $
    sameModule
      (Mod @DemoQuoted)
      (Mod @DemoExpected)

  it "quotes either / dict / tags / datetime / raw" $
    sameModule
      (Mod @KitchenQuoted)
      (Mod @KitchenExpected)

  it "quotes backtick-escaped keyword bind and ref" $
    sameModule
      (Mod @EscapedQuoted)
      (Mod @EscapedExpected)


sameModule
  :: Mod a
  -> Mod a
  -> IO ()
sameModule _ _ =
  pure ()


type TrivialQuoted =
  [jsonspec| module Id = string |]


type EscapedQuoted =
  [jsonspec|
    module X = let {
      module `let` = let {
        type `module` = int
        in `module`
      }
      type `string` = int
      in `string`
    }
  |]


type EscapedExpected =
  'Module
    (JsonLet
      '[ ModuleBind "let" (
           'Module (
             JsonLet
               '[ TypeBind "module" JsonInt ]
               (JsonRef "module")
           )
         )
       , TypeBind "string" JsonInt
       ]
       (JsonRef "string"))


type PersonQuoted =
  [jsonspec|
    module Person = let {
      type Person = {
        "name": string,
        "age": int,
        "email"?: null string
      }
      in Person
    }
  |]


type PersonExpected =
  'Module
    (JsonLet
      '[ TypeBind "Person"
          (JsonObject
            '[ Required "name" JsonString
             , Required "age" JsonInt
             , Optional "email" (JsonNullable JsonString)
             ])
       ]
      (JsonRef "Person"))


type DemoQuoted =
  [jsonspec|
    module Demo = let {
      type Id = string

      type Money = {
        "amount": number,
        "currency": string
      }

      -- Closed: cannot see Id or Money; define what it needs locally.
      module Tax = let {
        type Rate = number
        type Line = {
          "sku": string,
          "qty": int,
          "price": {
            "amount": number,
            "currency": string
          }
        }
        in {
          "rate": Rate,
          "lines": [Line]
        }
      }

      -- Open let: may use Id, Money, Tax from the enclosing frame.
      type Invoice = let {
        type Line = {
          "sku": string,
          "qty": int,
          "price": Money
        }
        in {
          "id": Id,
          "items": [Line],
          "tax": Tax,
          "notes"?: null string
        }
      }

      in Invoice
    }
  |]


type DemoExpected =
  'Module
    (JsonLet
      '[ TypeBind "Id" JsonString
       , TypeBind "Money"
          (JsonObject
            '[ Required "amount" JsonNum
             , Required "currency" JsonString
             ])
       , ModuleBind "Tax"
          ('Module
            (JsonLet
              '[ TypeBind "Rate" JsonNum
               , TypeBind "Line"
                  (JsonObject
                    '[ Required "sku" JsonString
                     , Required "qty" JsonInt
                     , Required "price"
                        (JsonObject
                          '[ Required "amount" JsonNum
                           , Required "currency" JsonString
                           ])
                     ])
               ]
              (JsonObject
                '[ Required "rate" (JsonRef "Rate")
                 , Required "lines" (JsonArray (JsonRef "Line"))
                 ])))
       , TypeBind "Invoice"
          (JsonLet
            '[ TypeBind "Line"
                (JsonObject
                  '[ Required "sku" JsonString
                   , Required "qty" JsonInt
                   , Required "price" (JsonRef "Money")
                   ])
             ]
            (JsonObject
              '[ Required "id" (JsonRef "Id")
               , Required "items" (JsonArray (JsonRef "Line"))
               , Required "tax" (JsonRef "Tax")
               , Optional "notes" (JsonNullable JsonString)
               ]))
       ]
      (JsonRef "Invoice"))


type KitchenQuoted =
  [jsonspec|
    module Kitchen = {
      "tag": "ok",
      "choice": either int | string | bool,
      "meta": dict datetime,
      "payload": null raw
    }
  |]


type KitchenExpected =
  'Module
    (JsonObject
      '[ Required "tag" (JsonTag "ok")
       , Required "choice" (JsonEither '[JsonInt, JsonString, JsonBool])
       , Required "meta" (JsonDict JsonDateTime)
       , Required "payload" (JsonNullable JsonRaw)
       ])


personSrc :: Text
personSrc =
  "module Person = let {\n\
  \  type Person = {\n\
  \    \"name\": string,\n\
  \    \"age\": int,\n\
  \    \"email\"?: null string\n\
  \  }\n\
  \  in Person\n\
  \}"


personAst :: Spec
personAst =
  LetSpec
    [ TypeBinding "Person"
        (ObjectSpec
          [ Field "name" False StringSpec
          , Field "age" False IntSpec
          , Field "email" True (NullSpec StringSpec)
          ])
    ]
    (RefSpec "Person")


graphsSrc :: Text
graphsSrc =
  "module Graphs = let {\n\
  \  type Node = {\n\
  \    \"id\": string,\n\
  \    \"edges\": [Edge]\n\
  \  }\n\
  \  type Edge = {\n\
  \    \"from\": Node,\n\
  \    \"to\": Node\n\
  \  }\n\
  \  in Node\n\
  \}"


graphsAst :: Spec
graphsAst =
  LetSpec
    [ TypeBinding "Node"
        (ObjectSpec
          [ Field "id" False StringSpec
          , Field "edges" False (ArraySpec (RefSpec "Edge"))
          ])
    , TypeBinding "Edge"
        (ObjectSpec
          [ Field "from" False (RefSpec "Node")
          , Field "to" False (RefSpec "Node")
          ])
    ]
    (RefSpec "Node")


demoSrc :: Text
demoSrc =
  "module Demo = let {\n\
  \  type Id = string\n\
  \  type Money = {\n\
  \    \"amount\": number,\n\
  \    \"currency\": string\n\
  \  }\n\
  \  module Tax = let {\n\
  \    type Rate = number\n\
  \    type Line = {\n\
  \      \"sku\": string,\n\
  \      \"qty\": int,\n\
  \      \"price\": {\n\
  \        \"amount\": number,\n\
  \        \"currency\": string\n\
  \      }\n\
  \    }\n\
  \    in {\n\
  \      \"rate\": Rate,\n\
  \      \"lines\": [Line]\n\
  \    }\n\
  \  }\n\
  \  type Invoice = let {\n\
  \    type Line = {\n\
  \      \"sku\": string,\n\
  \      \"qty\": int,\n\
  \      \"price\": Money\n\
  \    }\n\
  \    in {\n\
  \      \"id\": Id,\n\
  \      \"items\": [Line],\n\
  \      \"tax\": Tax,\n\
  \      \"notes\"?: null string\n\
  \    }\n\
  \  }\n\
  \  in Invoice\n\
  \}"


demoAst :: Spec
demoAst =
  LetSpec
    [ TypeBinding "Id" StringSpec
    , TypeBinding "Money"
        (ObjectSpec
          [ Field "amount" False NumberSpec
          , Field "currency" False StringSpec
          ])
    , ModuleBinding "Tax"
        (LetSpec
          [ TypeBinding "Rate" NumberSpec
          , TypeBinding "Line"
              (ObjectSpec
                [ Field "sku" False StringSpec
                , Field "qty" False IntSpec
                , Field "price" False
                    (ObjectSpec
                      [ Field "amount" False NumberSpec
                      , Field "currency" False StringSpec
                      ])
                ])
          ]
          (ObjectSpec
            [ Field "rate" False (RefSpec "Rate")
            , Field "lines" False (ArraySpec (RefSpec "Line"))
            ]))
    , TypeBinding "Invoice"
        (LetSpec
          [ TypeBinding "Line"
              (ObjectSpec
                [ Field "sku" False StringSpec
                , Field "qty" False IntSpec
                , Field "price" False (RefSpec "Money")
                ])
          ]
          (ObjectSpec
            [ Field "id" False (RefSpec "Id")
            , Field "items" False (ArraySpec (RefSpec "Line"))
            , Field "tax" False (RefSpec "Tax")
            , Field "notes" True (NullSpec StringSpec)
            ]))
    ]
    (RefSpec "Invoice")


kitchenSrc :: Text
kitchenSrc =
  "module Kitchen = {\n\
  \  \"tag\": \"ok\",\n\
  \  \"choice\": either int | string | bool,\n\
  \  \"meta\": dict datetime,\n\
  \  \"payload\": null raw\n\
  \}"


kitchenAst :: Spec
kitchenAst =
  ObjectSpec
    [ Field "tag" False (TagSpec "ok")
    , Field "choice" False
        (EitherSpec [IntSpec, StringSpec, BoolSpec])
    , Field "meta" False (DictSpec DateTimeSpec)
    , Field "payload" False (NullSpec RawSpec)
    ]


commentsSrc :: Text
commentsSrc =
  "module Comments = {- block -} {\n\
  \  \"a\": int, -- line comment\n\
  \  \"b\"?: string,\n\
  \}"
