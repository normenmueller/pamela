{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Cmdln
Description : ...
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Cmdln
    ( Opt(..)
    , cmdln
    ) where

import           Data.Semigroup        ((<>))
import           Data.Version          (showVersion)
import           Options.Applicative
import           Paths_pamela          (version)
import           Prelude               hiding (readFile)

data Opt =
    Opt
        { optTgt :: Maybe FilePath
-- FIXME        , optOrg :: Bool
        , optSrc :: FilePath
        }

cmdln :: IO Opt
cmdln =
    execParser $
    info
        (helper <*> veropt <*> opts)
        (fullDesc <>
         header "pamela - graPhify Archimate Model Exchange fiLe formAt")
  where
    veropt :: Parser (a -> a)
    veropt =
        infoOption (showVersion version) (long "version" <> help "Show version")

-- Note: As @program --global-options command --local-options@ is a fairly
-- standard pattern, we do not support @program command
-- --global-and-local-options@.
--
-- Note: Global options are not shown in sub-commands.
-- (cf. [#138](https://github.com/pcapriotti/optparse-applicative/issues/138))
opts :: Parser Opt
opts =
    Opt <$>
    optional
        (strOption
             (long "out" <>
              short 'o' <> metavar "TARGET" <> help "Target file path")) <*>
--    switch (long "with-orgs" <> help "With organizational structure") <*>
    argument str (metavar "SOURCE")
