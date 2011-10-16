
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parrow
-- Copyright   :  (c) Hans Höglund 2011
-- License     :  
--                           
-- Maintainer  :  hans@hanshoglund.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A reformulation of arrows using parallel as a primitive (rather than first).
-- 
-- Note: This module overrides the following prelude definitions:
--
-- @
--     id (>>) (||)
-- @

module Control.Parrow where

import Prelude hiding (id, (>>),(||),(++))

import qualified Control.Monad as M
import qualified Control.Monad.Fix as MF

infixr 3 ||
infixr 3 <>
infixr 2 ++
-- infixr 2 ><
infixr 1 <<, >>


-- | Basic parallell arrow class.
--
--   Minimal complete definition: 'lift', '>>' and '||'

class Parrow a where
    
    id   :: a b b                        
    lift :: (b -> c) -> a b c

    (>>) :: a b c -> a c d -> a b d
    (<<) :: a c d -> a b c -> a b d
    (||) :: a b c -> a b' c' -> a (b, b') (c, c')
    (<>) :: a b c -> a b c' -> a b (c, c')
    (>|) :: a b c -> a b c' -> a b c
    (|<) :: a b c -> a b c' -> a b c'
                                   
    first  :: a b c -> a (b, d) (c, d)
    second :: a b c -> a (d, b) (d, c)
                      
    id       = lift (\x->x)
    
    first  f = f  || id
    second f = id || f
        
    f << g = g >> f

    f <> g = lift dup >> f || g 
    f >| g = lift dup >> f || g >> lift fst
    f |< g = lift dup >> f || g >> lift snd

dup x = (x, x)


instance Parrow (->) where
    lift f = f
    f >> g = g . f
    (f || g) ~(x, y) = (f x, g y)


-- | Kleisli arrows of a monad.

data Kleisli m a b = Kleisli (a -> m b)

instance Monad m => Parrow (Kleisli m) where
    lift f = Kleisli (return . f)

    (Kleisli g) >> (Kleisli f) 
        = Kleisli (\x -> g x >>= f)

    (Kleisli g) || (Kleisli f) 
        = Kleisli $ \ ~(x, y) -> g x 
                      >>= \x' -> f y     
                      >>= \y' -> return (x', y')


-- | The identity arrow, which plays the role of 'return' in arrow notation.

returnA :: Parrow a => a b b
returnA = lift id


class Parrow a => ParrowZero a where
    zeroArrow :: a b c

instance M.MonadPlus m => ParrowZero (Kleisli m) where
    zeroArrow = Kleisli (\_ -> M.mzero)

class ParrowZero a => ParrowPlus a where
    (<+>) :: a b c -> a b c -> a b c

instance M.MonadPlus m => ParrowPlus (Kleisli m) where
    Kleisli f <+> Kleisli g = Kleisli (\x -> f x `M.mplus` g x)

-- | Choice, for arrows that support it.  This class underlies the
--   @if@ and @case@ constructs in arrow notation.
--   Minimal complete definition: '++'

class Parrow a => ParrowChoice a where

    left  :: a b c -> a (Either b d) (Either c d)
    right :: a b c -> a (Either d b) (Either d c)
    (++)  :: a b c -> a b' c' -> a (Either b b') (Either c c')
    (><)  :: a b d -> a c d -> a (Either b c) d

    left  f = f ++ id
    right f = id ++ f
    f >< g  = f ++ g >> lift untag
        where untag (Left x) = x
              untag (Right y) = y 


-- 
-- {-# RULES
-- "left/arr"      forall f .
--                 left (arr f) = arr (left f)
-- "right/arr"     forall f .
--                 right (arr f) = arr (right f)
-- "sum/arr"       forall f g .
--                 arr f +++ arr g = arr (f +++ g)
-- "fanin/arr"     forall f g .
--                 arr f ||| arr g = arr (f ||| g)
-- "compose/left"  forall f g .
--                 left f . left g = left (f . g)
-- "compose/right" forall f g .
--                 right f . right g = right (f . g)
--  #-}
-- 
-- instance ArrowChoice (->) where
--     left f = f +++ id
--     right f = id +++ f
--     f +++ g = (Left . f) ||| (Right . g)
--     (|||) = either
-- 
-- instance Monad m => ArrowChoice (Kleisli m) where
--     left f = f +++ arr id
--     right f = arr id +++ f
--     f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
--     Kleisli f ||| Kleisli g = Kleisli (either f g)
-- 
-- -- | Some arrows allow application of arrow inputs to other inputs.
-- 
-- class Arrow a => ArrowApply a where
--     app :: a (a b c, b) c
-- 
-- instance ArrowApply (->) where
--     app (f,x) = f x
-- 
-- instance Monad m => ArrowApply (Kleisli m) where
--     app = Kleisli (\(Kleisli f, x) -> f x)
-- 
-- -- | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
-- --   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.
-- 
-- newtype ArrowMonad a b = ArrowMonad (a () b)
-- 
-- instance ArrowApply a => Monad (ArrowMonad a) where
--     return x = ArrowMonad (arr (\_ -> x))
--     ArrowMonad m >>= f = ArrowMonad $
--         m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app
-- 
-- -- | Any instance of 'ArrowApply' can be made into an instance of
-- --   'ArrowChoice' by defining 'left' = 'leftApp'.
-- 
-- leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
-- leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
--              (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app
-- 
-- -- | The 'loop' operator expresses computations in which an output value is
-- --   fed back as input, even though the computation occurs only once.
-- --   It underlies the @rec@ value recursion construct in arrow notation.
-- 
-- class Arrow a => ArrowLoop a where
--     loop :: a (b,d) (c,d) -> a b c
-- 
-- instance ArrowLoop (->) where
--     loop f b = let (c,d) = f (b,d) in c
-- 
-- instance MonadFix m => ArrowLoop (Kleisli m) where
--     loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
--       where f' x y = f (x, snd y)
--                                       
