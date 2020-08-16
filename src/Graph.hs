{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Graph
Description : ...
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Graph
    ( module I
    , I.prettyPrint
    , fromDocument
    , toCypher
    ) where

import qualified Data.Graph.Inductive       as I
import qualified Data.Graph.Inductive.Graph as I
import           Data.Map                   ((!))
import qualified Data.Map                   as Map

import           AMX                  as A
import qualified XML                  as X

fromDocument :: X.Document -> I.Gr A.Elm A.Rel
fromDocument d =
    let (els, rls) = (A.elements d, A.relations d)
        ns = [1..] `zip` Map.elems els
        idx = Map.fromList $ (\(ni, e) -> (elmID e, ni)) <$> ns
        es = (\r -> (idx ! relSrc r, idx ! relTgt r, r)) <$> Map.elems rls
     in I.mkGraph ns es

toCypher :: I.Gr A.Elm A.Rel -> String
toCypher g =
    let ns = I.nodes g
        n0 = head ns
        ts = I.suc g n0
        xx = I.lab g <$> ts
     in concat
            [ "src:\n", show $ I.lab g n0, "\n"
            , "tgt:\n", show xx
            ]
