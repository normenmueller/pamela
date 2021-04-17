{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-|
Module      : Parser
Description : ...
Copyright   : (c) Normen Müller
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Data.Graph.Inductive.Parser
    ( fromAMX
    ) where

import qualified Data.Graph.Inductive       as I
import           Data.Map                   ((!))
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Text.XML.AMX               as A
import qualified Text.XML.Plain             as X

-- XXX Not `X.Document` but `A.Document`
fromAMX :: X.Document -> I.Gr A.Elm A.Rel
fromAMX d =
    let (els, rls) = (,) <$> A.elements <*> A.relationships $ d
        ns = [1..] `zip` Set.elems els
        idx = Map.fromList $ (\(i, e) -> (A.elmID e, i)) <$> ns
        es = (\r -> (idx ! A.relSrc r, idx ! A.relTgt r, r)) <$> Set.elems rls
     in I.mkGraph ns es
