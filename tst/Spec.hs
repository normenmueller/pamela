{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception   (evaluate)
import           Control.Monad.Trans
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Test.Hspec
import           Test.QuickCheck

import qualified XML as X
import qualified AMX as A

main :: IO ()
main = hspec $ do
    describe "Property definitions are" $
        it "parsed adequately" $
            X.withStripedSpaces "./tst/in0/in0.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("propid-1","author"),("propid-2","version")]
                  res = A.propertyDefinitions doc
              res `shouldBe` exp
    describe "Model properties are" $
        it "parsed adequately" $
            X.withStripedSpaces "./tst/in0/in0.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("author", "zzz"),("version", "0.1.0")]
                  res = A.properties doc
              res `shouldBe` exp
  where
    dft = X.def {X.psRetainNamespaces = True}
