module Pamela.CLI
    ( Opt(..)
    , cmdln
    ) where

import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative
import Paths_pamela (version)

data Opt =
    Opt
        { optTgt :: Maybe FilePath
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
    argument str (metavar "SOURCE")
