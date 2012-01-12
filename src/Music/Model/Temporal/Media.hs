{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Music.Model.Temporal.Media
(
  (>>>)
, (|||)
, (<<<)
, Delay(..)
, DurationFoldable(..)
, DurationFunctor(..)
, Joinable(..)
, TimeFoldable(..)
, TimeFunctor(..)
)
where

import Data.Monoid
import Data.Foldable

import Temporal.Media (Media, EventList(..), Event(..), eventContent, (+:+), (=:=))
import Temporal.Music.Notation (Time, Dur, rest, Score, renderScore)
import qualified Temporal.Media as M
import qualified Temporal.Music.Notation as N

infixl 5 >>>
infixl 5 <<<
infixl 3 |||
     
(>>>), (|||), (<<<) :: M.Dur t => Media t a -> Media t a -> Media t a

(>>>) = (+:+)
(|||) = (=:=)
a <<< b = b >>> a

-- | A weaker version of Foldable.
class Joinable j where
    join :: (Monoid m, Delay m) => j m -> m

class Delay a where
    delay :: Time -> a -> a

-- | A version of Functor including time.
class TimeFunctor f where
    tmap :: (Time -> a -> b) -> f a -> f b

-- | A version of Functor including duration.
class DurationFunctor f where
    dmap :: (Dur -> a -> b) -> f a -> f b

-- | A version of Foldable including time.
class TimeFoldable t where
    tfoldMap :: (Monoid m, Delay m) => (Time -> a -> m) -> t a -> m

-- | A version of Foldable including duration.
class DurationFoldable t where
    dfoldMap :: (Monoid m, Delay m) => (Dur -> a -> m) -> t a -> m


instance Monoid (Media Time a) where
    mempty  = rest 0
    mappend = (=:=)

instance Delay (Media Time a) where
    delay = M.delay

instance Joinable (Media Time) where
    join = mconcat . eventListToList . renderScore
        where eventListToList (EventList _ xs) = map (\(Event s d c) -> delay s c) xs


-- instance Foldable (Media Time) where
--     foldMap f = join . fmap f
--
instance TimeFunctor (Media Time) where
    tmap = M.tmap

instance DurationFunctor (Media Time) where
    dmap = M.dmap

instance TimeFoldable (Media Time) where
    tfoldMap f = join . tmap f

instance DurationFoldable (Media Time) where
    dfoldMap f = join . dmap f
