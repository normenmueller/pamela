module Pamela.CLI where

import Options.Applicative

data Opt =
    Opt
        { optSrc :: FilePath
        , optTgt :: Maybe FilePath
        }

cmdln :: IO Opt
cmdln = undefined
