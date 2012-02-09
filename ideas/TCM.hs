{-# LANGUAGE TypeSynonymInstances #-}
module TCM where          
    
import Control.Applicative

    
type Time  = Double         
type Time2 = Double

newtype B a = B { runB :: Time -> a }
newtype E     a = E     [(Time2, a)]


time :: B Time
time = B id

instance Functor B where
    fmap = undefined

instance Applicative B where
    pure a            = B (pure a) 
    (B fs) <*> (B as) = B (\t -> (fs t) (as t))