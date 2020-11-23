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

import qualified Data.Text.IO as TIO
import qualified Graph        as G
import           XML          (withStripedSpaces)
import qualified XML          as X

main :: IO ()
main =
   withStripedSpaces "./tst/in0.xml" $ \p -> do
        g <- G.fromDocument <$> X.parse p
        c <- pure $ G.toCypher g
        TIO.putStrLn c
