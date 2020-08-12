{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Lib
Description : ...
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Lib
    ( Log
    , Cfg(..)
    , Program
    , runProgram
    , pamela
    ) where

import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           AMX                  as A
import qualified Graph                as G
import           Graph                (E, Gr, V)
import qualified XML                  as X

type Log = [String]

type Program config log a = ReaderT config (WriterT log IO) a

newtype Cfg = Cfg
    { cfgPath :: FilePath
    } deriving (Eq, Ord, Show)

runProgram :: Program Cfg Log a -> Cfg -> IO (a, Log)
runProgram c = runWriterT . runReaderT c

type Pamela a = Program Cfg [String] a

pamela :: Pamela (Gr V E)
pamela = do
    f <- asks cfgPath

    doc <- liftIO $ X.readFile dft f
    tell . pure $ "File '" <> f <> "' parsed"

    -- XXX dedicated AMX env (ReaderT)
    -- propID -> Value
    let pds = A.propDefs doc
    -- elmID -> (elmId, elmName, elmType, elmProp)
    let els = A.elements pds doc
    -- relID -> (relId, relSrc, relTgt, relType, relProp)
    let rls = undefined

    let ns = (\e -> undefined :: V) <$> Map.elems els
    let es = (\r -> undefined :: E) <$> Map.elems rls

    let g = G.mkGraph ns es

    tell . pure . show $ pds
    tell . pure $ "-------"
    tell . pure . show $ els

    pure G.empty
  where
    dft = X.def {X.psRetainNamespaces = True}
