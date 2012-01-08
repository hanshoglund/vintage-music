{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}

{-|
    Standard model.
-}
module Music.Model.Standard
(
  Sequential(..)
, Parallel(..)  

, Behaviour(..)
, Event(..)
)
where

import Prelude hiding ((>>))
import Data.Monoid
import Data.Ratio     
import Data.Tree


-- | Sequencial music
class Sequential s where           
    instant :: s a
    (>>)    :: s a -> s a -> s a
    (<<)    :: s a -> s a -> s a
    a << b = b >> a

-- | Parallel music
class Parallel p where
    tacet   :: p a
    (||)    :: p a -> p a -> p a


type Time          = Double
data (Ord t, Bounded t) => Behaviour t a = Behaviour (t -> Maybe a)
data (Ord t, Bounded t) => Event     t a = Event     [(t, a)]


instance (Ord t, Bounded t) => Sequential (Behaviour t) where
    instant = Behaviour $ const Nothing
    (>>)    = undefined

instance (Ord t, Bounded t) => Sequential (Event t) where
    instant = Event $ []
    (>>)    = undefined


instance Parallel Tree where
    tacet = undefined
    (||)  = undefined

