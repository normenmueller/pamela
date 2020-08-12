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
    ( propDefs
    , elements
    ) where

import           Data.List
import           Data.Map                          (Map, (!))
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.String
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Text.XML                          (Document (..), Element (..),
                                                    Name (..), Node (..))
import           XML

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

-- |A key
newtype Key =
    Key
        { unKey :: Text
        }
    deriving (Eq, Ord, Show)

instance IsString Key where
    fromString = Key . T.pack

-- |A value
newtype Val =
    Val
        { unVal :: Text
        }
    deriving (Eq, Ord, Show)

instance IsString Val where
    fromString = Val . T.pack

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- |An element description
data Elm = Elm
    { elmID :: Eid
    , elmName :: Text
    , elmType :: Text
    , elmProp :: Map Key Val
    } deriving (Eq, Ord, Show)

{------------------------------------------------------------------------------
  Property Definitions
------------------------------------------------------------------------------}

propDefs :: Document -> Map Pid Val
propDefs d@Document {..} =
    case find (hasLabel lblPropDefs) (elementNodes documentRoot) of
        Just (NodeElement e) -> foldl op z $ elementNodes e
        Nothing -> Map.empty
  where
    z = Map.empty
    op m (NodeElement e) = Map.union m (propDef e)
    op m _ = m

-- Note: According to AMX XSD, `name` is the only child of an property
-- definition.
propDef :: Element -> Map Pid Val
propDef (Element l as [c])
    | l == lblPropDef = Map.singleton (Pid $ as ! attId) (Val $ text c)
propDef _ = Map.empty

{------------------------------------------------------------------------------
  Element description
------------------------------------------------------------------------------}

elements :: Map Pid Val -> Document -> Map Eid Elm
elements pds d@Document {..} =
    case find (hasLabel lblElems) (elementNodes documentRoot) of
        Just (NodeElement e) -> foldl op z $ elementNodes e
        Nothing -> Map.empty
  where
    z = Map.empty
    op m (NodeElement e) = Map.union m (element pds e)
    op m _ = m

-- Note: According to AMX XSD, `name` is the first child of an element.
element :: Map Pid Val -> Element -> Map Eid Elm
element pds (Element l as (n:ps))
    | l == lblElem =
        Map.singleton (Eid $ as ! attId) $
        Elm (Eid $ as ! attId) (text n) (as ! attType) (props pds ps)
element _ _ = Map.empty

props ::  Map Pid Val -> [Node] -> Map Key Val
props pds ns =
    case find (hasLabel lblProps) ns of
        Just (NodeElement e) -> foldl op z $ elementNodes e
        Nothing -> z
  where
    z = Map.empty
    op m (NodeElement e) = Map.union m (prop pds e)
    op m _ = m

prop :: Map Pid Val -> Element -> Map Key Val
prop pds (Element l as [c])
    | l == lblProp =
        Map.singleton
            (Key . unVal $ pds ! (Pid $ as ! attPropDefRef))
            (Val $ text c)
prop _ _ = Map.empty

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

-- Unsafe!
text :: Node -> T.Text
text n = fromMaybe (error "Invalid AMX!") (content n)

{------------------------------------------------------------------------------
  Terminals
------------------------------------------------------------------------------}

-- Attributes

attId :: Name
attId = "identifier"

attType :: Name
attType =
    Name
        "type"
        (Just "http://www.w3.org/2001/XMLSchema-instance")
        (Just "xsi")

attPropDefRef :: Name
attPropDefRef = "propertyDefinitionRef"

-- Labels

lblElems :: Name
lblElems =
    Name
        "elements"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblElem :: Name
lblElem =
    Name
        "element"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblPropDefs :: Name
lblPropDefs =
    Name
        "propertyDefinitions"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblPropDef :: Name
lblPropDef =
    Name
        "propertyDefinition"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblProps :: Name
lblProps =
    Name
        "properties"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblProp :: Name
lblProp =
    Name
        "property"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing

lblName :: Name
lblName =
    Name
        "name"
        (Just "http://www.opengroup.org/xsd/archimate/3.0/")
        Nothing
