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
import           Data.List
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T

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
    let srcs = I.nodes g
        src = head srcs
        sinf :: Maybe A.Elm
        sinf = I.lab g src
        tgts = I.suc g src
     in concat
            [ "src:\n", show $ cqlElm <$> I.lab g src, "\n"
            , "------------\n"
            , "tgts:\n", show $ I.lab g <$> tgts
            ]

cqlElm :: A.Elm -> Text
cqlElm e =
    let var = elmName e
        typ = elmType e
        ps  = elmProp e
        ps' = Map.assocs ps
        ps''  = T.intercalate (T.pack ",") $ (\(k,v) -> k <> T.pack ":'" <> v <> T.pack "'") <$> ps'
     in T.pack "(`" <> var <> T.pack "`:" <> typ <> T.pack " {" <> ps'' <> T.pack "})"
