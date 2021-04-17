{-|
Module      : Commons
Description : ...
Copyright   : (c) Normen Müller
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Data.Commons where

import Control.Monad (mfilter)
import Data.Maybe

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace a b = fmap $ fromMaybe b . mfilter (/= a) . Just
