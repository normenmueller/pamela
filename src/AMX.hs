{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : AMX
Description : ...
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module AMX
    ( Eid(..)
    , Elm(..)
    , Rel(..)
    , Key
    , Val
    , elements
    , relationships
    , propertyDefinitions
    ) where

import           Control.Monad.Trans.State
import           Data.List                  (find)
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree
import qualified XML                        as X

{------------------------------------------------------------------------------
  Type synonyms
------------------------------------------------------------------------------}

-- |Property identifier
newtype Pid =
    Pid
        { unPid :: Text
        }
    deriving (Eq, Ord, Show)

instance IsString Pid where
    fromString = Pid . T.pack

-- |Element identifier
newtype Eid =
    Eid
        { unEid :: Text
        }
    deriving (Eq, Ord, Show)

instance IsString Eid where
    fromString = Eid . T.pack

-- |Relationship identifier
newtype Rid =
    Rid
        { unRid :: Text
        }
    deriving (Eq, Ord, Show)

instance IsString Rid where
    fromString = Rid . T.pack

-- |A key
type Key = Text
--newtype Key =
--    Key
--        { unKey :: Text
--        }
--    deriving (Eq, Ord, Show)
--
--instance IsString Key where
--    fromString = Key . T.pack

-- |A value
type Val = Text
--newtype Val =
--    Val
--        { unVal :: Text
--        }
--    deriving (Eq, Ord, Show)
--
--instance IsString Val where
--    fromString = Val . T.pack

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- |An element description
data Elm =
    Elm
        { elmID :: Eid
        , elmName :: Text
        , elmType :: Text
        , elmProp :: Map Key Val
        }
    deriving (Eq, Ord, Show)

-- |A relationship description
data Rel =
    Rel
        { relID :: Rid
        , relName :: Maybe Text
        , relType :: Text
        , relSrc :: Eid
        , relTgt :: Eid
        , relProp :: Map Key Val
        }
    deriving (Eq, Ord, Show)

{------------------------------------------------------------------------------
  Element description
------------------------------------------------------------------------------}

elements :: X.Document -> Set Elm
elements d@X.Document {..} =
     case find (X.hasLabel lblElems) (X.elementNodes documentRoot) of
          Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
          Nothing -> Set.empty
  where
    z = Set.empty
    op m (X.NodeElement e) = Set.union m (element (propertyDefinitions d) e)
    op m _ = m

element :: Map Pid Val -> X.Element -> Set Elm
element pds (X.Element l as cs)
    | l == lblElem =
        case find (X.hasLabel lblName) cs of
            Just n ->
                Set.singleton $
                Elm (Eid $ as ! attId) (text n) (as ! attType) (props pds cs)
            Nothing -> error "Invalid AMX!"
    | otherwise = Set.empty

{------------------------------------------------------------------------------
  Relationship description
------------------------------------------------------------------------------}

relationships :: X.Document -> Set Rel
relationships d@X.Document {..} =
    case find (X.hasLabel lblRels) (X.elementNodes documentRoot) of
        Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
        Nothing -> Set.empty
  where
    z = Set.empty
    op m (X.NodeElement e) =
        Set.union m (relationship (propertyDefinitions d) e)
    op m _ = m

relationship :: Map Pid Val -> X.Element -> Set Rel
relationship pds (X.Element l as cs)
    | l == lblRel =
        Set.singleton $
        Rel
            (Rid $ as ! attId)
            (text <$> find (X.hasLabel lblName) cs)
            (as ! attType)
            (Eid $ as ! attSrc)
            (Eid $ as ! attTgt)
            (props pds cs)
    | otherwise = Set.empty

{------------------------------------------------------------------------------
  Property definitions
------------------------------------------------------------------------------}

propertyDefinitions :: X.Document -> Map Pid Val
propertyDefinitions X.Document {..} =
    case find (X.hasLabel lblPropDefs) (X.elementNodes documentRoot) of
        Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
        Nothing -> Map.empty
  where
    z = Map.empty
    op m e = Map.union m (propertyDefinition e)

propertyDefinition :: X.Node -> Map Pid Val
propertyDefinition (X.NodeElement (X.Element l as cs))
    | l == lblPropDef =
        case find (X.hasLabel lblName) cs of
            Just v -> Map.singleton (Pid $ as ! attId) (text v)
            Nothing -> Map.empty
propertyDefinition _ = Map.empty

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

--name :: X.Node -> Maybe T.Text
--name (X.NodeElement (X.Element l _ [c]))
--    | l == lblName = X.content c
--name _ = Nothing

-- Unsafe!
text :: X.Node -> T.Text
text n = fromMaybe (error "Invalid AMX!") (X.content n)

{------------------------------------------------------------------------------
  Element rsp. relationship properties
------------------------------------------------------------------------------}

props ::  Map Pid Val -> [X.Node] -> Map Key Val
props pds ns =
    case find (X.hasLabel lblProps) ns of
        Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
        Nothing -> z
  where
    z = Map.empty
    op m (X.NodeElement e) = Map.union m (prop pds e)
    op m _ = m

prop :: Map Pid Val -> X.Element -> Map Key Val
prop pds (X.Element l as [c])
    | l == lblProp = Map.singleton (pds ! Pid (as ! attPropDefRef)) (text c)
prop _ _ = Map.empty

{------------------------------------------------------------------------------
  Terminals
------------------------------------------------------------------------------}

-- Attributes

attId :: X.Name
attId = "identifier"

attIdRef :: X.Name
attIdRef = "identifierRef"

attSrc :: X.Name
attSrc = "source"

attTgt :: X.Name
attTgt = "target"

attType :: X.Name
attType =
    X.Name
        "type"
        (Just "http://www.w3.org/2001/XMLSchema-instance")
        (Just "xsi")

attPropDefRef :: X.Name
attPropDefRef = "propertyDefinitionRef"

-- Labels

lblOrgs :: X.Name
lblOrgs =
    X.Name
        "organizations"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblItm :: X.Name
lblItm =
    X.Name
        "item"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblElems :: X.Name
lblElems =
    X.Name
        "elements"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblElem :: X.Name
lblElem =
    X.Name
        "element"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblRels :: X.Name
lblRels =
    X.Name
        "relationships"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblRel :: X.Name
lblRel =
    X.Name
        "relationship"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblPropDefs :: X.Name
lblPropDefs =
    X.Name
        "propertyDefinitions"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblPropDef :: X.Name
lblPropDef =
    X.Name
        "propertyDefinition"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblProps :: X.Name
lblProps =
    X.Name
        "properties"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblProp :: X.Name
lblProp =
    X.Name
        "property"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblName :: X.Name
lblName =
    X.Name
        "name"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

--lblLabel :: X.Name
--lblLabel =
--    X.Name
--        "label"
--        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
--        Nothing
