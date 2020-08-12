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
    , mkGraph
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

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Node =
    Node
        { gId   :: Text
        , nId   :: Text
        , nName :: Text
        , nType :: Text
        , nProp :: Map Text Text
        }
    deriving (Eq, Ord, Show)

type V = Node

data Edge =
    Edge
        { eId   :: Text
        , eSrc  :: Text -- ^Node.nId
        , eTgt  :: Text -- ^Node.nId
        , eType :: Text
        , ePorp :: Map Text Text
        }
    deriving (Eq, Ord, Show)

type E = Edge

-- G.mkGraph :: [LNode a] -> [LEdge b] -> Gr a b
-- type Node = Int
-- type LNode a = (Node, a)
-- type LEdge b = (Node, Node, b)
mkGraph :: [V] -> [E] -> Gr V E
mkGraph vs es =
    let vs' :: [G.LNode Node]
        vs' = [1..] `zip` vs
        -- nId -> gId
        m :: Map Text Int
        m = undefined
        e :: Edge
        e = undefined
        es' :: [G.LEdge Edge]
        es' =
            foldl
                (\acc e ->
                     let src = m ! eSrc e
                         tgt = m ! eTgt e
                      in (src, tgt, e) : acc)
                []
                es
     in G.mkGraph vs' es'

--sg :: IO ()
--sg = prettyPrint g
--  where
--    g :: Gr String String
--    g =
--        mkGraph
--            [(1, "nlA"), (2, "nlB"), (3, "nlC")]
--            [(1, 2, "ab"), (1, 3, "ac"), (3, 2, "cb")]
