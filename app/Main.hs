{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Main entry point
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

Pamela's entry point.
-}
module Main where

import           Cmdln
import           Control.Monad ((=<<))
import           Data.SemVer
import qualified Data.Text.IO  as TIO
import qualified Graph         as G
import           Options.Applicative
import           XML           (withStripedSpaces)
import qualified XML           as X

{------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

main :: IO ()
main = pamela =<< cmdln

{------------------------------------------------------------------------------
  Punk
-------------------------------------------------------------------------------}

pamela :: Opt -> IO ()
pamela (Opt to from) =
    withStripedSpaces from $ \p -> do
         g <- G.fromDocument <$> X.parse p
         c <- pure $ G.toCypher g
         case to of
            Just to -> TIO.writeFile to c
            Nothing -> TIO.putStrLn c
