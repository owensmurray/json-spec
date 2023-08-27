{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.JsonSpec (Field(Field), HasJsonDecodingSpec(DecodingSpec,
  fromJSONStructure), HasJsonEncodingSpec(EncodingSpec, toJSONStructure),
  Rec(Rec, unRec), SpecJSON(SpecJSON), Specification(JsonArray,
  JsonDateTime, JsonEither, JsonInt, JsonLet, JsonNullable, JsonNum,
  JsonObject, JsonRef, JsonString, JsonTag), Tag(Tag))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime(UTCTime))
import Prelude (Applicative(pure), Either(Left, Right), Enum(toEnum),
  Maybe(Just, Nothing), Traversable(traverse), ($), (.), Eq, IO, Int,
  Show, String, realToFrac)
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Data.Aeson as A


main :: IO ()
main =
  hspec $ do
    describe "json" $ do
      it "encodes product" $
        let
          actual :: ByteString
          actual = A.encode $ sampleTestObject
          expected :: ByteString
          expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100}"
        in
          actual `shouldBe` expected

      it "decodes product" $
        let
          actual :: Either String TestObj
          actual =
            A.eitherDecode
              "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":100}"
          expected :: Either String TestObj
          expected = Right sampleTestObject
        in
          actual `shouldBe` expected

      it "encodes sum1" $
        let
          actual :: ByteString
          actual = A.encode $ TestA 0 "bar"
          expected :: ByteString
          expected = "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
        in
          actual `shouldBe` expected

      it "encodes sum2" $
        let
          actual :: ByteString
          actual = A.encode $ TestB
          expected :: ByteString
          expected = "{\"tag\":\"b\"}"
        in
          actual `shouldBe` expected

      it "decodes sum1" $
        let
          actual :: Either String TestSum
          actual =
            A.eitherDecode
              "{\"content\":{\"int-field\":0,\"txt-field\":\"bar\"},\"tag\":\"a\"}"
          expected :: Either String TestSum
          expected = Right (TestA 0 "bar")
        in
          actual `shouldBe` expected

      it "decodes sum2" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"b\"}"
          expected :: Either String TestSum
          expected = Right TestB
        in
          actual `shouldBe` expected

      it "decodes UTCTime" $
        let
          actual :: Either String User
          actual =
            A.eitherDecode
              "{ \"name\": \"foo\", \"last-login\": \"1858-11-17T00:00:00Z\" }"

          expected :: Either String User
          expected =
            Right
              User
                { name = "foo"
                , lastLogin =
                    UTCTime (toEnum 0) 0
                }
        in
          actual `shouldBe` expected

      describe "let" $ do
        it "decodes let" $
          let
            actual :: Either String Triangle
            actual =
              A.eitherDecode
                "{ \"vertex1\" : { \"x\": 1, \"y\": 2, \"z\": 3 }, \
                \  \"vertex2\" : { \"x\": 4, \"y\": 5, \"z\": 6 }, \
                \  \"vertex3\" : { \"x\": 7, \"y\": 8, \"z\": 9 } }"

            expected :: Either String Triangle
            expected =
              Right
                Triangle
                  { vertex1 = Vertex 1 2 3
                  , vertex2 = Vertex 4 5 6
                  , vertex3 = Vertex 7 8 9
                  }
          in
            actual `shouldBe` expected
        it "encodes let" $
            let
              actual :: ByteString
              actual =
                A.encode
                  Triangle
                    { vertex1 = Vertex 1 2 3
                    , vertex2 = Vertex 4 5 6
                    , vertex3 = Vertex 7 8 9
                    }

              expected :: ByteString
              expected = "{\"vertex1\":{\"x\":1,\"y\":2,\"z\":3},\"vertex2\":{\"x\":4,\"y\":5,\"z\":6},\"vertex3\":{\"x\":7,\"y\":8,\"z\":9}}"
            in
              actual `shouldBe` expected

      describe "recursive types" $ do
        it "decodes" $
          let
            actual :: Either String LabelledTree
            actual =
              A.eitherDecode
                "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"

            expected :: Either String LabelledTree
            expected =
              Right
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
          in
            actual `shouldBe` expected
        it "decodes" $
          let
            actual :: ByteString
            actual =
              A.encode
                LabelledTree
                  { label = "grandparent"
                  , children =
                      [ LabelledTree
                          { label = "parent"
                          , children =
                              [ LabelledTree
                                  { label = "child1"
                                  , children = []
                                  }
                              , LabelledTree
                                  { label = "child2"
                                  , children = []
                                  }
                              ]
                          }
                      ]
                  }
            expected :: ByteString
            expected = "{\"children\":[{\"children\":[{\"children\":[],\"label\":\"child1\"},{\"children\":[],\"label\":\"child2\"}],\"label\":\"parent\"}],\"label\":\"grandparent\"}"
          in
            actual `shouldBe` expected
          

      describe "nullable" $ do
        it "encodes product" $
          let
            actual :: ByteString
            actual = A.encode $ sampleTestObjectWithNull
            expected :: ByteString
            expected = "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null}"
          in
            actual `shouldBe` expected

        it "decodes product" $
          let
            actual :: Either String TestObj
            actual =
              A.eitherDecode
                "{\"bar\":1,\"baz\":{\"bar\":0,\"foo\":\"foo2\"},\"foo\":\"foo\",\"qux\":null}"
            expected :: Either String TestObj
            expected = Right sampleTestObjectWithNull
          in
            actual `shouldBe` expected

      it "Bad tag does not decode" $
        let
          actual :: Either String TestSum
          actual = A.eitherDecode "{\"tag\":\"c\"}"
          expected :: Either String TestSum
          expected = Left "Error in $: unexpected constant value"
        in
          actual `shouldBe` expected


sampleTestObject :: TestObj
sampleTestObject =
  TestObj
    { foo = "foo"
    , bar = 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }
    , qux = Just 100
    }


sampleTestObjectWithNull:: TestObj
sampleTestObjectWithNull=
  TestObj
    { foo = "foo"
    , bar = 1
    , baz =
        TestSubObj
          { foo2 = "foo2"
          , bar2 = 0
          }

    , qux = Nothing
    }


data TestSum
  = TestA Int Text
  | TestB
  deriving stock (Eq, Show)
  deriving ToJSON via (SpecJSON TestSum)
  deriving FromJSON via (SpecJSON TestSum)
instance HasJsonEncodingSpec TestSum where
  type EncodingSpec TestSum =
    JsonEither
      (JsonObject '[
        '("tag", JsonTag "a"),
        '("content", JsonObject [
          '("int-field", JsonInt),
          '("txt-field", JsonString)
        ])
      ])
      (JsonObject '[
        '("tag", JsonTag "b")
      ])
  toJSONStructure = \case
    TestA i t ->
      Left
        (Field @"tag" (Tag @"a"),
        (Field @"content"
          ( (Field @"int-field" i
          , (Field @"txt-field" t
          , ()
          )
        )),
        ()))
    TestB ->
      Right
        ( Field @"tag" (Tag @"b")
        , ()
        )
instance HasJsonDecodingSpec TestSum where
  type DecodingSpec TestSum = EncodingSpec TestSum
  fromJSONStructure = \case
    Left
        (Field @"tag" Tag,
        (Field @"content"
          (Field @"int-field" int,
          (Field @"txt-field" txt,
          ())),
        ()))
      ->
        pure (TestA int txt)
    Right _ ->
      pure TestB


data TestObj = TestObj
  { foo :: Text
  , bar :: Scientific
  , baz :: TestSubObj
  , qux :: Maybe Int
  }
  deriving stock (Show, Eq)
  deriving ToJSON via (SpecJSON TestObj)
  deriving FromJSON via (SpecJSON TestObj)
instance HasJsonEncodingSpec TestObj where
  type EncodingSpec TestObj =
    JsonObject
      '[
        '("foo", JsonString),
        '("bar", JsonNum),
        '("baz", EncodingSpec TestSubObj),
        '("qux", JsonNullable JsonInt)
      ]
  toJSONStructure TestObj { foo , bar , baz, qux } =
    (Field @"foo" foo,
    (Field @"bar" (realToFrac bar),
    (Field @"baz" (toJSONStructure baz),
    (Field @"qux" qux,
    ()))))
instance HasJsonDecodingSpec TestObj where
  type DecodingSpec TestObj = EncodingSpec TestObj
  fromJSONStructure
      (Field @"foo" foo,
      (Field @"bar" bar,
      (Field @"baz" rawBaz,
      (Field @"qux" qux,
      ()))))
    = do
      baz <- fromJSONStructure rawBaz
      pure $ TestObj { foo, bar, baz, qux }


data TestSubObj = TestSubObj
  { foo2 :: Text
  , bar2 :: Int
  }
  deriving stock (Show, Eq)
instance HasJsonEncodingSpec TestSubObj where
  type EncodingSpec TestSubObj =
    JsonObject
      '[ '("foo", JsonString)
       , '("bar", JsonInt)
       ]
  toJSONStructure TestSubObj { foo2 , bar2 } =
    (Field @"foo" foo2,
    (Field @"bar" bar2,
    ()))
instance HasJsonDecodingSpec TestSubObj where
  type DecodingSpec TestSubObj = EncodingSpec TestSubObj
  fromJSONStructure
      (Field @"foo" foo2,
      (Field @"bar" bar2,
      ()))
    =
      pure TestSubObj {foo2 , bar2}


data User = User
  { name :: Text
  , lastLogin :: UTCTime
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON User)
instance HasJsonEncodingSpec User where
  type EncodingSpec User =
    JsonObject
      '[ '("name", JsonString)
       , '("last-login", JsonDateTime)
       ]
  toJSONStructure user =
    (Field @"name" (name user),
    (Field @"last-login" (lastLogin user),
    ()))
instance HasJsonDecodingSpec User where
  type DecodingSpec User = EncodingSpec User
  fromJSONStructure
      (Field @"name" name,
      (Field @"last-login" lastLogin,
      ()))
    =
      pure User { name , lastLogin }


data Vertex = Vertex
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Vertex)
instance HasJsonEncodingSpec Vertex where
  type EncodingSpec Vertex =
    JsonObject
      '[ '("x", JsonInt)
       , '("y", JsonInt)
       , '("z", JsonInt)
       ]
  toJSONStructure Vertex {x, y, z} =
    (Field @"x" x,
    (Field @"y" y,
    (Field @"z" z,
    ())))
instance HasJsonDecodingSpec Vertex where
  type DecodingSpec Vertex = EncodingSpec Vertex
  fromJSONStructure
      (Field @"x" x,
      (Field @"y" y,
      (Field @"z" z,
      ())))
    =
      pure Vertex { x, y, z }


data Triangle = Triangle
  { vertex1 :: Vertex
  , vertex2 :: Vertex
  , vertex3 :: Vertex
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON Triangle)
instance HasJsonEncodingSpec Triangle where
  type EncodingSpec Triangle =
    JsonLet
      '[ '("Vertex", EncodingSpec Vertex) ]
      (JsonObject
        '[ '("vertex1", JsonRef "Vertex")
         , '("vertex2", JsonRef "Vertex")
         , '("vertex3", JsonRef "Vertex")
         ])
  toJSONStructure Triangle {vertex1, vertex2, vertex3} =
    (Field @"vertex1" (toJSONStructure vertex1),
    (Field @"vertex2" (toJSONStructure vertex2),
    (Field @"vertex3" (toJSONStructure vertex3),
    ())))
instance HasJsonDecodingSpec Triangle where
  type DecodingSpec Triangle = EncodingSpec Triangle
  fromJSONStructure
      (Field @"vertex1" rawVertex1,
      (Field @"vertex2" rawVertex2,
      (Field @"vertex3" rawVertex3,
      ())))
    = do
      vertex1 <- fromJSONStructure rawVertex1
      vertex2 <- fromJSONStructure rawVertex2
      vertex3 <- fromJSONStructure rawVertex3
      pure Triangle{vertex1, vertex2, vertex3}


data LabelledTree = LabelledTree
  {    label :: Text
  , children :: [LabelledTree]
  }
  deriving stock (Show, Eq)
  deriving (ToJSON, FromJSON) via (SpecJSON LabelledTree)
instance HasJsonEncodingSpec LabelledTree where
  type EncodingSpec LabelledTree =
      JsonLet
        '[ '("LabelledTree",
               JsonObject
                 '[ '("label", JsonString)
                  , '("children", JsonArray (JsonRef "LabelledTree"))
                  ]
            )
         ]
        (JsonRef "LabelledTree")
  toJSONStructure LabelledTree {label , children } =
    (Field @"label" label,
    (Field @"children"
      [ Rec (toJSONStructure child)
      | child <- children
      ],
    ()))
instance HasJsonDecodingSpec LabelledTree where
  type DecodingSpec LabelledTree = EncodingSpec LabelledTree
  fromJSONStructure
      (Field @"label" label,
      (Field @"children" children_,
      ()))
    = do
      children <- traverse (fromJSONStructure . unRec) children_
      pure LabelledTree { label , children }


