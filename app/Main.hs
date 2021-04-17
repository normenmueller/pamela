{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Main entry point
Copyright   : (c) Normen Müller
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

Pamela's entry point.
-}
module Main where

import           CLI.Opts
import           Control.Monad                 ((=<<))
import qualified Data.Graph.Inductive.Parser   as G
import qualified Data.Graph.Inductive.Renderer as G
import           Data.SemVer
import qualified Data.Text.IO                  as TIO
import           Text.XML.Plain                (withStripedSpaces)
import qualified Text.XML.Plain                as X

{------------------------------------------------------------------------------
  Entry
-------------------------------------------------------------------------------}

main :: IO ()
main = pamela =<< options

{------------------------------------------------------------------------------
  Punk
-------------------------------------------------------------------------------}

pamela :: Opt -> IO ()
pamela (Opt to from) =
    withStripedSpaces from $ \f -> do
         g <- G.fromAMX <$> X.parse f
         let c = G.toCypher g
         case to of
            Just to -> TIO.writeFile to c
            Nothing -> TIO.putStrLn c
