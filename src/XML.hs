{-|
Module      : XML
Description : Utilities for pure XML. No further underlying semantics assumed.
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module XML
    ( module X
    , withStripedSpaces
    , parse
    , hasLabel
    , label
    , content
    , remove
    ) where

import qualified Data.Text            as T
import           Prelude              hiding (readFile)
import qualified System.Directory     as Sys
import qualified System.Exit          as Sys (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath      ((</>))
import qualified System.IO            as Sys
import qualified System.IO.Temp       as Sys
import qualified System.Process.Typed as Sys
import           Text.XML             as X

{------------------------------------------------------------------------------
  Pre
------------------------------------------------------------------------------}

-- |Strip any white space between elements.
--
-- XXX Evil hack! It couldn't be worse! At least we support brackets ;-)
--
-- Note, whitespace processing must be done /before/ further XML processing.
withStripedSpaces :: FilePath -> (FilePath -> IO ()) -> IO ()
withStripedSpaces res act = do
    tmpDir <- Sys.getCanonicalTemporaryDirectory
    let xsltFile = tmpDir </> "cleanup.xslt"
    Sys.writeFile xsltFile xsltScript
    let res' = tmpDir </> res
    exitCode <- Sys.runProcess (Sys.proc "xsltproc" ["-o", res', xsltFile, res])
    case exitCode of
        (Sys.ExitFailure e) -> error . show $ e
        Sys.ExitSuccess -> do
            act res'
            Sys.removeFile xsltFile
            Sys.removeFile res'
  where
    xsltScript = "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" ><xsl:output method=\"xml\" indent=\"no\"/><xsl:strip-space elements=\"*\"/><xsl:template match=\"/\"><xsl:copy-of select=\".\"/></xsl:template></xsl:stylesheet>"

{------------------------------------------------------------------------------
  Parser
------------------------------------------------------------------------------}

parse :: FilePath -> IO Document
parse = readFile def {psRetainNamespaces = True}

{------------------------------------------------------------------------------
  Predicates
------------------------------------------------------------------------------}

hasLabel :: X.Name -> X.Node -> Bool
hasLabel la (X.NodeElement (X.Element lb _ _)) = la == lb
hasLabel _ _ = False

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

label :: X.Node -> Maybe X.Name
label (X.NodeElement (X.Element l _ _)) = Just l
label _ = Nothing

content :: X.Node -> Maybe T.Text
content (X.NodeElement (X.Element _ _ [c])) = content c
content (X.NodeContent txt) = Just txt
content _ = Nothing

{------------------------------------------------------------------------------
  Deletion/ Update
------------------------------------------------------------------------------}

remove :: (X.Node -> Bool) -> X.Node -> X.Node
remove f (X.NodeElement (X.Element l as cs)) =
    X.NodeElement (X.Element l as (remove f <$> filter (not . f) cs))
remove _ n = n
