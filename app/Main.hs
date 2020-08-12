{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Main entry point
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

Pamela's entry point.

```
  render . transform . parse
~ dump   . valet     . slurp
```

-}
module Main where

import qualified AMX as A
import qualified Graph as G
import qualified XML as X
import           XML (withStripedSpaces)
import           Lib

main :: IO ()
main =
    withStripedSpaces "./tst/in0.xml" $ \p -> do
        (g, log) <- runProgram pamela $ Cfg p
        G.prettyPrint g
        mapM_ putStrLn log
