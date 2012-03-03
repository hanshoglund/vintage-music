{-|
    Module      :  Music.Time
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
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

-- * Timed values
    Time(..),
    time2Double,
-- ** Duration
    Timed(..),
-- ** Position
    Delayed(..),
-- ** Split
    Split(..),

-- -- * Transformers
--     TemporalTrans(..)
    
)
where

import Data.Monoid

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
concatSeq :: Temporal d => [d a] -> d a

-- | Parallel concatenation.
concatPar :: Temporal d => [d a] -> d a

concatSeq = Prelude.foldr (>>>) instant
concatPar = Prelude.foldr (|||) instant


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
class (Enum t, Ord t, Real t, Fractional t) => Time t where

instance Time Double
instance Time Rational

time2Double :: Time t => t -> Double
time2Double = fromRational . toRational


-- | Values with a duration.
class Time t => Timed t d | d -> t where
    duration :: d a -> t
    stretch  :: t -> d a -> d a


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


-- | Instances of 'Loop', 'Split' and 'Timed' should satisfy
--
--   > before d (after d) (loop x) = a
--   >     where d = duration x * n
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
