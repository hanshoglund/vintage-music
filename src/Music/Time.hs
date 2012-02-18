{-|
    Module      :  Music.Time
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FunctionalDependencies #-}

module Music.Time
(
-- * Temporal values
    Temporal(..),
-- ** Loop
    Loop(..),
-- ** Reverse
    Reverse(..),

-- * Timed values
    Time(..),
-- ** Duration
    Timed(..),
-- ** Position
    Delayed(..),
-- ** Split
    Split(..)
)
where

infixr 9 <<<
infixr 9 >>>
infixr 8 |||

------------------------
-- Temporal values
------------------------

{-|
    Composable temporal values.

    Laws:

    * `instant` should form a monoid with each of the operations `>>>`, `|||` and `<<<`.

    * Instances of `Temporal` and `Timed` should satisfy the following laws:

    > duration (a >>> b) = duration a + duration b
    > duration (a <<< b) = duration a + duration b
    > duration (a ||| b) = duration a `max` duration b
    >
    > a >>> b ||| c >>> d = (a ||| c) >>> (c ||| d) only if P, where P is one of
    >   duration a = duration c
    >   duration b = duration d
    >   duration a = duration b = duration c = duration d


    Minimal complete definition: all except `>>>` or `<<<`.
-}
class Temporal t where
    -- | The instantanous temporal value.
    instant :: t a

    -- | Parallel composition of temporal values.
    (|||)   :: t a -> t a -> t a

    -- | Sequential composition of temporal values.
    (>>>)   :: t a -> t a -> t a

    -- | Reverse sequential composition of temporal values.
    (<<<)   :: t a -> t a -> t a

    a <<< b = a >>> b
    a >>> b = a <<< b


{-|
    Instances of `Loop` and `Timed` should satisfy the following law
    whenever `Timed` is parameterized with an instance of `Bounded`:

    > duration (loop x) = maxBound
-}
class Temporal t => Loop t where
    loop :: t a -> t a
    loop x = x >>> loop x


{-|
    Reverse is an involution, so the following equation must always hold:

    > (reverse . reverse) = id

    Instances of `Reverse` and `Timed` should also satisfy the following laws:

    > duration (reverse x) = duration x
-}
class Temporal t => Reverse t where
    reverse :: t a -> t a


------------------------
-- Timed values
------------------------

{-|
    Time values must be ordered and support numeric operations.
-}
class (Ord t, Num t) => Time t where

instance Time Double
instance Time Integer
instance Time Rational


{-|
    Values with a duration.
-}
class Time t => Timed t d | d -> t where
    duration :: d a -> t
    stretch  :: t -> d a -> d a

{-|
    Values with an offset.

    Instances should satisfy the following laws:

    > delay t x = rest t >>> x
    > a >>> b = a ||| delay d b
    >     where d = duration a
-}
class Time t => Delayed t d | d -> t where
    rest   :: t -> d a
    delay  :: t -> d a -> d a
    -- offset :: d a -> t


{-|
    Instances of 'Loop', 'Split' and 'Timed' should satisfy

    > before d (after d) (loop x) = a
    >     where d = duration x * n

    for any natural number @n@.

   Minimal complete definition: either `split` or all except `split`.
-}
class Time t => Split t d | d -> t where
    split  :: t -> d a -> (d a, d a)
    before :: t -> d a -> d a
    after  :: t -> d a -> d a

    split t x = (before t x, after t x)

    before t x = a where (a, b) = split t x
    after  t x = b where (a, b) = split t x

