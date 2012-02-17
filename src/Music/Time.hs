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
-- * Temporal class
    Temporal(..),

-- * Manipulation of temporal values
-- ** Looping
    LoopTemporal(..),
-- ** Reversing
    ReverseTemporal(..),
-- ** Cutting
    SplitTemporal(..),

-- * Offset and duration
    Time(..),
    Timed(..),
    Delayed(..),
)
where

infixr 9 <<<
infixr 9 >>>
infixr 8 |||


-- | Value with temporal semantics. Minimal complete definition: `instant`, `>>>` and `|||`.
--   
--   * `instant` and `|||` should form a monoid, as should `instant` and `>>>`:
--   
--   > instant >>> x         = x
--   > x       >>> instant   = x
--   >
--   > instant ||| x         = x
--   > x       ||| instant   = x
--   >
--   > x >>> (y >>> z) = (x >>> y) >>> z
--   > x ||| (y ||| z) = (x ||| y) ||| z
--   
--   * Instances for `Timed` should also satisfy the following laws:
--   
--   > duration (x >>> y) = duration x + duration y
--   > duration (x <<< y) = duration x + duration y
--   > duration (x ||| y) = duration x `max` duration y
--
class Temporal e where 
    -- | The instantanous temporal value. 
    instant :: e a

    -- | Parallel composition of temporal values. 
    (|||)   :: e a -> e a -> e a

    -- | Sequential composition of temporal values. 
    (>>>)   :: e a -> e a -> e a

    -- | Reverse sequential composition of temporal values. 
    (<<<)   :: e a -> e a -> e a
    a <<< b = a >>> b


-- | Time values must be ordered and support numeric operations.
class (Ord t, Num t) => Time t where
instance Time Double


-- | Values with a duration.
class Time t => Timed t d where
    duration        :: d a -> t

    stretch         :: t -> d a -> d a
    stretch t       = withDuration (* t)

    withDuration    :: (t -> t) -> d a -> d a -- FIXME this should go

-- | Values with an offset.
class (Time t, Temporal d) => Delayed t d where
    rest    :: t -> d a

    delay   :: t -> d a -> d a
    delay t x = rest t >>> x


-- | Values that can be looped.
class (Time t, Temporal d) => LoopTemporal t d | d -> t where

    -- | Loop the given value for ever.
    loop    :: d a -> d a
    loop x  = x >>> loop x


-- | Values that can be reversed.
--
--   * Instances should satisfy the following laws:
--   
--   > (reverse . reverse) = id
--
class (Time t, Temporal d) => ReverseTemporal t d | d -> t   where

    -- | Reverse the given value.
    reverse :: d a -> d a


-- | Values that can be cut.
--
--   * Instances of 'LoopTemporal' and 'Timed' should satisfy the following laws:
--   
--   > before d (loop x)           = x
--   > before d (after d) (loop x) = x
--   >     where d = duration x
--
--   Minimal complete definition: either `split` or all the others.
class (Time t, Temporal d) => SplitTemporal t d | d -> t   where    
    
    split   :: t -> d a -> (d a, d a, d a) 
    before  :: t -> d a -> d a
    at      :: t -> d a -> d a
    after   :: t -> d a -> d a
    
    split t x = (before t x, at t x, after t x)

    before t x = a where (a, b, c) = split t x
    at     t x = b where (a, b, c) = split t x
    after  t x = c where (a, b, c) = split t x     
 
                                                             