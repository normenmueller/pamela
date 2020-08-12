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
    ( V
    , E
    , module G
    , G.empty
    , G.prettyPrint
    ) where

import           Data.Graph.Inductive              (Gr)
import qualified Data.Graph.Inductive              as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import           Data.Map                          (Map, (!))
import qualified Data.Map                          as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as T

data NodeLabel =
    NodeLabel
        { vName :: Text
        , vProp :: Map Text Text
        }
    deriving (Eq, Ord, Show)

type V = NodeLabel

data EdgeLabel =
    EdgeLabel
        { eName :: Text
        , ePorp :: Map Text Text
        }
    deriving (Eq, Ord, Show)

type E = EdgeLabel

--sg :: IO ()
--sg = prettyPrint g
--  where
--    g :: Gr String String
--    g =
--        mkGraph
--            [(1, "nlA"), (2, "nlB"), (3, "nlC")]
--            [(1, 2, "ab"), (1, 3, "ac"), (3, 2, "cb")]
