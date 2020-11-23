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

import qualified Data.Graph.Inductive       as I
import           Data.List.Split
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           AMX                  as A
import qualified XML                  as X

-- TODO Add 'organizations'
--fromDocument :: X.Document -> I.Gr A.Elm A.Rel
--fromDocument d =
--    let (els, rls) = (,) <$> A.elements <*> A.relationships $ d
--        ns = [1..] `zip` Map.elems els
--        idx = Map.fromList $ (\(i, e) -> (elmID e, i)) <$> ns
--        es = (\r -> (idx ! relSrc r, idx ! relTgt r, r)) <$> Map.elems rls
--     in I.mkGraph ns es

fromDocument :: X.Document -> I.Gr A.Elm A.Rel
fromDocument d =
    let (els, rls) = (,) <$> A.elements' <*> A.relationships' $ d
        ns = [1..] `zip` Set.elems els
        idx = Map.fromList $ (\(i, e) -> (elmID e, i)) <$> ns
        es = (\r -> (idx ! relSrc r, idx ! relTgt r, r)) <$> Set.elems rls
     in I.mkGraph ns es

toCypher :: I.Gr A.Elm A.Rel -> Text
toCypher g = "CREATE " <> T.intercalate "," ((++) <$> cqlElms <*> cqlRels $ g)

cqlElms :: I.Gr A.Elm A.Rel -> [Text]
cqlElms g = foldl (\acc (_, e) -> cqlElm e : acc) [] $ I.labNodes g

cqlElm :: A.Elm -> Text
cqlElm e =
    let id = T.replace "-" "_" $ A.unEid . elmID $ e
        ty = elmType e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$>
            ("name", elmName e) : Map.assocs (elmProp e)
     in "(" <> id <> ":" <> ty <> " {" <> ps <> "})"

cqlRels :: I.Gr A.Elm A.Rel -> [Text]
cqlRels g = foldl (\acc (_, _, r) -> cqlRel r : acc) [] $ I.labEdges g

cqlRel :: A.Rel -> Text
cqlRel e =
    let id = T.replace "-" "_" $ fromMaybe T.empty $ relName e
        ty = relType e
        src = T.replace "-" "_" $ A.unEid . relSrc $ e
        tgt = T.replace "-" "_" $ A.unEid . relTgt $ e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$> Map.assocs (relProp e)
     in "(" <> src <> ")" <>
        "-[" <> id <> ":" <> ty <> " {" <> ps <> "}]->" <>
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
