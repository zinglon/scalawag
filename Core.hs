module Core ( module Control.Applicative
            , module Control.Monad
            , Arrow(..), ArrowChoice(..), (>>>), (<<<)
            , catMaybes, mapMaybe, fromMaybe, isJust, isNothing, maybeToList
            , Word32(..)
            , module Data.Record.Label
            , module Core
            ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Word

import Data.Record.Label


data Void

type Ticks = Word32


