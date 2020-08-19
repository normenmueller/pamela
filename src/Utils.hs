{-|
Module      : Utils
Description : ...
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Utils where

import Control.Monad (mfilter)
import Data.Maybe

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace a b = fmap $ fromMaybe b . mfilter (/= a) . Just
