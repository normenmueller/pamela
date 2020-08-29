{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception   (evaluate)
import           Control.Monad.Trans
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Test.Hspec
import           Test.QuickCheck
import Data.List.Utils (replace)

import qualified XML as X
import qualified AMX as A

main :: IO ()
main = hspec $ do
    describe "Property definitions are" $
        it "read in adequately" $
            X.withStripedSpaces "./tst/in0.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let exp = Map.fromList [("propid-1","author"),("propid-2","version"),("propid-3","relatesTo"),("propid-4","status"),("propid-5","context"),("propid-6","scope")]
                  res = A.propDefs doc
              res `shouldBe` exp
    -- XXX Evil equality :-( To be removed!
    describe "Elements are" $
        it "read in adequately" $
            X.withStripedSpaces "./tst/in0.xml" $ \amx -> do
              doc <- liftIO $ X.readFile dft amx
              let res = A.elements doc
              exp <- liftIO $ readFile "./tst/exp0.txt"

              let exp' = replace "\\\"" "\"" $ init exp
              let res' = replace "\\\"" "\"" $ show res

              res' `shouldBe` exp'
  where
    dft = X.def {X.psRetainNamespaces = True}
