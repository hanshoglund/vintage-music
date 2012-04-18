{-|
    Module      :  Music.Time
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
    
    This module defines type clases for temporal and durational values.
      Basic temporal compositions are provided by the 'Temporal' class.
      Durational properties are split into the two classes 'Timed' and
      'Delayed'.
    
      The special temporal operations 'loop' and 'reverse' are defined in
      their own classes instead of 'Temporal', as not all instances support
      them. The same goes for the 'split' operation, which are supported by
      some, but not all instances of 'Timed' and 'Delayed'.

-}

{-# LANGUAGE               
    TypeSynonymInstances,
    MultiParamTypeClasses,
    FunctionalDependencies #-}

module Music.Time
(
-- * Temporal values
    Temporal(..),
    concatSeq,
    concatPar,
    Seq(..),
    Par(..),
-- ** Loop
    Loop(..),
-- ** Reverse
    Reverse(..),

-- * Durational values
    Time(..),
    timeToDouble,
    timeToRational,
-- ** Position
    Delayed(..),
-- ** Duration
    Timed(..),
-- ** Split
    Split(..), 
    
-- * Miscellaneous
    anticipate,
    sustain,
    prolong,
    restBefore,
    restAfter,
    restBoth,
    stretchTo
)
where

import Prelude hiding ( foldr )
import Data.Monoid
import Data.Foldable ( Foldable, foldr )

infixr 9 <<<
infixr 9 >>>
infixr 8 |||

--
-- Temporal values
--

--  Composable temporal values.
--  
--  Laws:
--  
--  * `instant` should form a monoid with each of the operations `>>>`, `|||` and `<<<`.
--  
--  * Instances of `Temporal` and `Timed` should satisfy the following laws:
--  
--  > duration (a >>> b) = duration a + duration b
--  > duration (a <<< b) = duration a + duration b
--  > duration (a ||| b) = duration a `max` duration b
--  
--  Minimal complete definition: all except `>>>` or `<<<`.

class Temporal d where
    -- | The instantanous temporal value.
    instant :: d a

    -- | Parallel composition of temporal values.
    (|||)   :: d a -> d a -> d a

    -- | Sequential composition of temporal values.
    (>>>)   :: d a -> d a -> d a

    -- | Reverse sequential composition of temporal values.
    (<<<)   :: d a -> d a -> d a

    a <<< b = a >>> b
    a >>> b = a <<< b


-- | Sequential concatenation.
concatSeq :: (Foldable f, Temporal d) => f (d a) -> d a

-- | Parallel concatenation.
concatPar :: (Foldable f, Temporal d) => f (d a) -> d a

concatSeq = foldr (>>>) instant
concatPar = foldr (|||) instant


-- | Monoid under sequential composition.
newtype Seq d a = Seq { getSeq :: d a }

instance Temporal d => Monoid (Seq d a) where
    mempty = Seq instant
    Seq a `mappend` Seq b = Seq (a >>> b)

-- | Monoid under parallell composition.
newtype Par d a = Par { getPar :: d a }

instance Temporal d => Monoid (Par d a) where
    mempty = Par instant
    Par a `mappend` Par b = Par (a ||| b)


-- | Instances of `Loop` and `Timed` should satisfy the following law
--   whenever `Timed` is parameterized with an instance of `Bounded`:
-- 
--   > duration (loop x) = maxBound

class Temporal d => Loop d where
    loop :: d a -> d a
    loop x = x >>> loop x

-- | Reverse is an involution, so the following equation must always hold:
--
--   > (reverse . reverse) = id
--   
--   Instances of `Reverse` and `Timed` should also satisfy the following laws:
--   
--   > duration (reverse x) = duration x

class Temporal d => Reverse d where
    reverse :: d a -> d a


--
-- Timed values
--

-- | Time values must be ordered and support fractional arithmetic.
class (Enum t, RealFrac t) => Time t where

instance Time Double
instance Time Rational

timeToDouble :: Time t => t -> Double
timeToDouble = fromRational . toRational

timeToRational :: Time t => t -> Rational
timeToRational = toRational

-- | Values with a duration.
class Time t => Timed t d | d -> t where
    duration :: d a -> t
    stretch  :: t -> d a -> d a
    compress :: t -> d a -> d a
    
    stretch  t  =  compress (1 / t)
    compress t  =  stretch  (1 / t)
        

-- | Values with an offset.
--
--  Instances should satisfy the following laws:
--
--  > delay t x = rest t >>> x
--  > a >>> b = a ||| delay d b
--  >     where d = duration a

class Time t => Delayed t d | d -> t where
    rest   :: t -> d a
    delay  :: t -> d a -> d a


-- | Values that can be split.
--
--   All instances should satisfy:
--
--   > split t x = (before t x, after t x)
--
--   Instances of 'Loop', 'Split' and 'Timed' should satisfy
--
--   > before d' (after d) (loop x) = a
--   >     where d  = duration x * n
--   >           d' = duration x * n + 1
--
--   for any natural number @n@.
--
--  Minimal complete definition: either `split` or all except `split`.

class Time t => Split t d | d -> t where
    split  :: t -> d a -> (d a, d a)
    before :: t -> d a -> d a
    after  :: t -> d a -> d a

    split t x = (before t x, after t x)
    before t x = a where (a, b) = split t x
    after  t x = b where (a, b) = split t x


    
-- | Like '|||', but stretching the second agument to the duration of the first.
sustain :: (Time t, Temporal d, Timed t d, Delayed t d) => d a -> d a -> d a
sustain x y = x ||| stretchTo (duration x) y

-- | Like '|||', but shortening the second agument to the duration of the first.
prolong :: (Time t, Temporal d, Timed t d, Split t d) => d a -> d a -> d a
prolong x y = x ||| before (duration x) y

-- | Like '>>>' but with a negative delay on the second element.
anticipate :: (Time t, Temporal d, Timed t d, Delayed t d) => t -> d a -> d a -> d a
anticipate t x y = x ||| delay t' y
    where
        t' = (duration x - t) `max` 0


-- | Prepend a rest of the given duration.
restBefore :: (Time t, Temporal d, Timed t d, Delayed t d) => t -> d a -> d a
restBefore = delay

-- | Append a rest of the given duration.
restAfter :: (Time t, Temporal d, Timed t d, Delayed t d) => t -> d a -> d a
restAfter t x
    | t <= 0     =  x
    | otherwise  =  x >>> r
    where r = rest t

-- | Prepend and append a rest of half the given duration.
restBoth :: (Time t, Temporal d, Timed t d, Delayed t d) => t -> d a -> d a
restBoth t x
    | t <= 0     =  x
    | otherwise  =  r >>> x >>> r
    where 
        r = rest (t / 2)

-- | Stretch to the given duration.
stretchTo :: (Time t, Temporal d, Timed t d, Delayed t d) => t -> d a -> d a
stretchTo t x 
    | duration x == t  =  x
    | otherwise        =  stretch (t / duration x) x 