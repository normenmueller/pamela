{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception   (evaluate)
import           Control.Monad.Trans
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Test.Hspec
import           Test.QuickCheck
import qualified Text.XML.Plain as X
import qualified Text.XML.AMX as A

main :: IO ()
main = hspec $ do
    describe "Property definitions are" $ do
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/mprop/none.xml"
              let exp = Map.empty
                  res = A.propertyDefinitions doc
              res `shouldBe` exp
        it "parsed adequately (obsolete)" $
            X.withStripedSpaces "./tst/data/mprop/some.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("propid-1","author"),("propid-2","version")]
                  res = A.propertyDefinitions doc
              res `shouldBe` exp
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/mprop/some.xml"
              let exp = Map.fromList [("propid-1","author"),("propid-2","version")]
                  res = A.propertyDefinitions doc
              res `shouldBe` exp
    describe "Model properties are" $ do
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/mprop/none.xml"
              let exp = Map.empty
                  res = A.properties doc
              res `shouldBe` exp
        it "parsed adequately (obsolete)" $
            X.withStripedSpaces "./tst/data/mprop/some.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("author", "zzz"),("version", "0.1.0")]
                  res = A.properties doc
              res `shouldBe` exp
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/mprop/some.xml"
              let exp = Map.fromList [("author", "zzz"),("version", "0.1.0")]
                  res = A.properties doc
              res `shouldBe` exp
    describe "DC metadata are" $ do
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/dc/none.xml"
              let exp = Map.empty
                  res = A.metadata doc
              res `shouldBe` exp
        it "parsed adequately (obsolete)" $
            X.withStripedSpaces "./tst/data/dc/some.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("schema", "Dublin Core"), ("schemaversion", "1.1"), ("title", "A title"), ("date", "A date")]
                  res = A.metadata doc
              res `shouldBe` exp
        it "parsed adequately" $
            do
              doc <- liftIO $ X.readFile dft "./tst/data/dc/some.xml"
              let exp = Map.fromList [("schema", "Dublin Core"), ("schemaversion", "1.1"), ("title", "A title"), ("date", "A date")]
                  res = A.metadata doc
              res `shouldBe` exp
        it "parsed adequately (obsolete)" $
            X.withStripedSpaces "./tst/data/dc/full.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("schema", "Dublin Core"), ("schemaversion", "1.1"), ("title", "A title"), ("creator", "A creator"), ("subject", "A subject"), ("description", "A description"), ("publisher", "A publisher"), ("contributor", "A contributor"), ("date", "A date"), ("type", "A type"), ("format", "A format"), ("identifier", "An identifier"), ("source", "A source"), ("language", "A language"), ("relation", "A relation"), ("coverage", "A coverage"), ("rights", "A right")]
                  res = A.metadata doc
              res `shouldBe` exp
  where
    dft = X.def {X.psRetainNamespaces = True}
