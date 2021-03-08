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
    ( Pid(..)
    , Eid(..)
    , Elm(..)
    , Rel(..)
    , Key
    , Val
    , AMX.metadata
    , properties
    , elements
    , relationships
    , propertyDefinitions
    ) where

import           Control.Monad.Trans.State
import           Data.List                  (find)
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.SemVer
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree
import qualified XML                        as X
import           Text.XML.Cursor            ((&/), ($/), (>=>))
import qualified Text.XML.Cursor            as XC

{------------------------------------------------------------------------------
  Type synonyms
------------------------------------------------------------------------------}

-- |Property identifier
type Pid = Text
--newtype Pid =
--    Pid
--        { unPid :: Text
--        }
--    deriving (Eq, Ord, Show)
--
--instance IsString Pid where
--    fromString = Pid . T.pack

-- |Element identifier
type Eid = Text
--newtype Eid =
--    Eid
--        { unEid :: Text
--        }
--    deriving (Eq, Ord, Show)
--
--instance IsString Eid where
--    fromString = Eid . T.pack

-- |Relationship identifier
type Rid = Text
--newtype Rid =
--    Rid
--        { unRid :: Text
--        }
--    deriving (Eq, Ord, Show)
--
--instance IsString Rid where
--    fromString = Rid . T.pack

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

-- |Metadata
--data Metadata =
--    DublinCore
--        { dcSchema        :: Text
--        , dcSchemaVersion :: Version
--        , dcTitle         :: Maybe Text
--        , dcCreator       :: Maybe Text
--        , dcSubject       :: Maybe Text
--        , dcDescription   :: Maybe Text
--        , dcPublisher     :: Maybe Text
--        , dcContributor   :: Maybe Text
--        , dcDate          :: Maybe Text
--        , dctype          :: Maybe Text
--        , dcFormat        :: Maybe Text
--        , dcIdentifier    :: Maybe Text
--        , dcSource        :: Maybe Text
--        , dcLanguage      :: Maybe Text
--        , dcRelation      :: Maybe Text
--        , dcCoverage      :: Maybe Text
--        , dcRights        :: Maybe Text
--        }

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
  Properties description
------------------------------------------------------------------------------}

-- |Model properties
-- If the property key is already present in the map, the associated value is
-- replaced with the supplied value. In other words, if a property key occurs
-- more than once, the last occurrence, according to the XML document structure,
-- wins.
properties :: X.Document -> Map Key Val
properties d@X.Document {..} =
    case find (X.hasLabel lblProps) (X.elementNodes documentRoot) of
        Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
        Nothing -> Map.empty
  where
    z = Map.empty
    op m e =
        maybe
            m
            (\(k, v) -> Map.insert k v m)
            (property (propertyDefinitions d) e)

-- Semantically equivalent to 'properties' but realized with XPath axis
properties' :: X.Document -> Map Key Val
properties' d@X.Document {..} =
    Map.fromList
        (catMaybes $
         property (propertyDefinitions d) . XC.node <$>
         (model $/ axProperties &/ axProperty))
  where
    model = XC.fromDocument d
    axProperties = XC.element lblProps
    axProperty = XC.element lblProp
    axValue = XC.element lblVal

property :: Map Pid Val -> X.Node -> Maybe (Key, Val)
property pds (X.NodeElement (X.Element l as cs))
    | l == lblProp =
        case find (X.hasLabel lblVal) cs of
            Just e -> do
                let k = pds ! (as ! attPropDefRef)
                v <- X.content e
                return (k, v)
            Nothing -> Nothing
    | otherwise = Nothing
property _ _ = Nothing

{------------------------------------------------------------------------------
  DC Metadata description
------------------------------------------------------------------------------}

-- |Model metadata
metadata :: X.Document -> Map Key Val
metadata d@X.Document {..} =
    case find (X.hasLabel lblMeta) (X.elementNodes documentRoot) of
        Just (X.NodeElement e) -> foldl op z $ X.elementNodes e
        Nothing -> Map.empty
  where
    z = Map.empty
    op m e = maybe m (\(k, v) -> Map.insert k v m) (dcelem e)

dcelem :: X.Node -> Maybe (Key, Val)
dcelem e@(X.NodeElement (X.Element l as cs)) = do
    v <- X.content e
    return (X.nameLocalName l, v)
dcelem _ = Nothing

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
                Elm (as ! attId) (text n) (as ! attType) (props pds cs)
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
            (as ! attId)
            (text <$> find (X.hasLabel lblName) cs)
            (as ! attType)
            (as ! attSrc)
            (as ! attTgt)
            (props pds cs)
    | otherwise = Set.empty

{------------------------------------------------------------------------------
  Organizations description
------------------------------------------------------------------------------}

organizations :: X.Document -> Tree Text
organizations = undefined

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
    op m e = maybe m (\(k,v) -> Map.insert k v m) (propertyDefinition e)

propertyDefinition :: X.Node -> Maybe (Pid, Val)
propertyDefinition (X.NodeElement (X.Element l as cs))
    | l == lblPropDef = do
        n <- find (X.hasLabel lblName) cs
        v <- X.content n
        return (as ! attId, v)
    | otherwise = Nothing
propertyDefinition _ = Nothing

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
    | l == lblProp = Map.singleton (pds ! (as ! attPropDefRef)) (text c)
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

lblMeta :: X.Name
lblMeta =
    X.Name
        "metadata"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

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

lblVal :: X.Name
lblVal =
    X.Name
        "value"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

--lblLabel :: X.Name
--lblLabel =
--    X.Name
--        "label"
--        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
--        Nothing
