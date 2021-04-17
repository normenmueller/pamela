{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Renderer
Description : ...
Copyright   : (c) Normen Müller
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Data.Graph.Inductive.Renderer
    ( toCypher
    ) where

import qualified Data.Graph.Inductive       as I
import           Data.List.Split
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Text.XML.AMX               as A
import qualified Text.XML.Plain             as X

toCypher :: I.Gr A.Elm A.Rel -> Text
toCypher g = "CREATE " <> T.intercalate "," ((++) <$> cqlElms <*> cqlRels $ g)

cqlElms :: I.Gr A.Elm A.Rel -> [Text]
cqlElms g = foldl (\acc (_, e) -> cqlElm e : acc) [] $ I.labNodes g

cqlElm :: A.Elm -> Text
cqlElm e =
    let id = T.replace "-" "_" $ A.elmID e
        ty = A.elmType e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$>
            ("name", A.elmName e) : Map.assocs (A.elmProp e)
     in "(" <> id <> ":" <> ty <> " {" <> ps <> "})"

cqlRels :: I.Gr A.Elm A.Rel -> [Text]
cqlRels g = foldl (\acc (_, _, r) -> cqlRel r : acc) [] $ I.labEdges g

cqlRel :: A.Rel -> Text
cqlRel e =
    let id = T.replace "-" "_" $ fromMaybe T.empty $ A.relName e
        ty = A.relType e
        src = T.replace "-" "_" $ A.relSrc e
        tgt = T.replace "-" "_" $ A.relTgt e
        ps =
            T.intercalate "," $
            (\(k, v) -> k <> ":" <> cqlPropV v) <$> Map.assocs (A.relProp e)
     in "(" <> src <> ")" <>
        "-[" <> id <> ":" <> ty <> " {" <> ps <> "}]->" <>
        "(" <> tgt <> ")"

newtype NoQuotes =
    NoQuotes String

instance Show NoQuotes where
    show (NoQuotes str) = str

cqlPropV :: Text -> Text
cqlPropV x =
    case splitOn "," $ T.unpack x of
        [v] -> "'" <> (T.pack . trim $ v) <> "'"
        vs -> T.pack . show $ (\v -> NoQuotes $ "'" <> trim v <> "'") <$> vs

trim :: String -> String
trim = T.unpack . T.strip . T.pack
--trim = f . f
--   where f = reverse . dropWhile isSpace
