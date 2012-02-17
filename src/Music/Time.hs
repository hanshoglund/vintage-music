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
-- * Temporal
    Temporal(..),
-- ** Loop
    Loop(..),
-- ** Reverse
    Reverse(..),

-- * Timed values
    Time(..),
-- ** Offset
    Delayed(..),
-- ** Duration
    Timed(..),
-- ** Split
    Split(..),

)
where

infixr 9 <<<
infixr 9 >>>
infixr 8 |||


{-| 
    Value with temporal semantics.
      
    * `instant` and should form a monoid with each of the operations `>>>`, `|||` and `<<<`.

    * Instances for `Timed` should also satisfy the following laws:

    > duration (x >>> y) = duration x + duration y
    > duration (x <<< y) = duration x + duration y
    > duration (x ||| y) = duration x `max` duration y

    Minimal complete definition: all except `>>>` or `<<<`.
-}
class Temporal t where 
    -- | The instantanous temporal value. 
    instant :: t a

    -- | Parallel composition of temporal values. 
    (|||)   :: t a -> t a -> t a

    -- | Sequential composition of temporal values. 
    (>>>)   :: t a -> t a -> t a
    a >>> b = a <<< b

    -- | Reverse sequential composition of temporal values. 
    (<<<)   :: t a -> t a -> t a
    a <<< b = a >>> b


{-| 
    Value that can be looped.
-}
class Temporal t => Loop t where
    -- | Loop the given value for ever.
    loop    :: t a -> t a
    -- loop x  = x >>> loop x


{-| 
    Value that can be reversed.

      * Instances should satisfy the following laws:
  
      > (reverse . reverse) = id

      * Instances for `Timed` should also satisfy the following laws:
  
      > duration (reverse x) = duration x
-}
class Temporal t => Reverse t where
    -- | Reverse the given value.
    reverse :: t a -> t a



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



-- | Values that can be cut.
--
--   * Instances of 'Loop' and 'Timed' should satisfy the following laws:
--   
--   > before d (loop x)           = x
--   > before d (after d) (loop x) = x
--   >     where d = duration x
--
--   Minimal complete definition: either `split` or all the others.
class (Time t, Temporal d) => Split t d | d -> t   where    
    
    split   :: t -> d a -> (d a, d a, d a) 
    before  :: t -> d a -> d a
    at      :: t -> d a -> d a
    after   :: t -> d a -> d a
    
    split t x = (before t x, at t x, after t x)

    before t x = a where (a, b, c) = split t x
    at     t x = b where (a, b, c) = split t x
    after  t x = c where (a, b, c) = split t x     
 
                                                             