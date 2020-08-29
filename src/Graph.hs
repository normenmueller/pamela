{-# LANGUAGE OverloadedStrings #-}
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
    ( fromDocument
    , toCypher
    ) where

import           Control.Monad              (mfilter)
import           Data.Char                  (isSpace)
import qualified Data.Graph.Inductive       as I
import qualified Data.Graph.Inductive.Graph as I
import           Data.List
import           Data.List.Split
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           AMX                  as A
import qualified XML                  as X
import           Utils

fromDocument :: X.Document -> I.Gr A.Elm A.Rel
fromDocument d =
    let (els, rls) = (,) <$> A.elements <*> A.relations $ d
        ns = [1..] `zip` Map.elems els
        idx = Map.fromList $ (\(ni, e) -> (elmID e, ni)) <$> ns
        es = (\r -> (idx ! relSrc r, idx ! relTgt r, r)) <$> Map.elems rls
     in I.mkGraph ns es

toCypher :: I.Gr A.Elm A.Rel -> Text
toCypher g = "CREATE " <> T.intercalate "," ((++) <$> cqlElms <*> cqlRels $ g)

cqlElms :: I.Gr A.Elm A.Rel -> [Text]
cqlElms g = foldl (\acc (_, e) -> cqlElm e : acc) [] $ I.labNodes g

cqlElm :: A.Elm -> Text
cqlElm e =
    let v = T.replace "-" "_" $ A.unEid . elmID $ e
        y = elmType e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$>
            ("name", elmName e) : Map.assocs (elmProp e)
     in "(" <> v <> ":" <> y <> " {" <> ps <> "})"

cqlRels :: I.Gr A.Elm A.Rel -> [Text]
cqlRels g = foldl (\acc (_, _, r) -> cqlRel r : acc) [] $ I.labEdges g

cqlRel :: A.Rel -> Text
cqlRel e =
    let v = T.replace "-" "_" $ fromMaybe T.empty $ relName e
        y = relType e
        src = T.replace "-" "_" $ A.unEid . relSrc $ e
        tgt = T.replace "-" "_" $ A.unEid . relTgt $ e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$> Map.assocs (relProp e)
     in "(" <> src <> ")" <>
        "-[" <> v <> ":" <> y <> " {" <> ps <> "}]->" <>
        "(" <> tgt <> ")"

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where show (NoQuotes str) = str

cqlPropV :: A.Val -> Text
cqlPropV x =
    case splitOn "," $ T.unpack x of
        [v] -> "'" <> (T.pack . trim $ v) <> "'"
        vs -> T.pack . show $ (\v -> NoQuotes $ "'" <> trim v <> "'") <$> vs

trim :: String -> String
trim = T.unpack . T.strip . T.pack
--trim = f . f
--   where f = reverse . dropWhile isSpace
