{-|
    Module      :  Data.Trivial
    Copyright   :  Hans Höglund 2005

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Data.Trivial where
    
import Control.Monad
import Data.Monoid

-- | A class for types with a trivial value.     
class Trivial a where
    trivial :: a
    
-- | Always a value, using trivial if needed.
always      :: Trivial a => Maybe a -> a

-- | Always select the left branch, using trivial if needed.
alwaysLeft  :: Trivial a => Either a b -> a

-- | Always select the right branch, using trivial if needed.
alwaysRight :: Trivial a => Either b a -> a

-- | Converts a partial function to a total function.
partial     :: Trivial a  => (a -> b) -> Maybe a -> b

-- | Converts a total function to a partial function.
total       :: Trivial b => (a -> Maybe b) -> a -> b

partialLeft   :: Trivial a  => (a -> b) -> Either a c -> b
partialRight  :: Trivial c  => (c -> b) -> Either a c -> b

totalLeft     :: Trivial b => (a -> Either b c) -> a -> b
totalRight    :: Trivial c => (a -> Either b c) -> a -> c

always (Nothing)      = trivial
always (Just x)       = x

alwaysLeft  (Left x)  = x
alwaysLeft  (Right _) = trivial

alwaysRight (Right x) = x
alwaysRight (Left _)  = trivial

partial f      = f . always
partialLeft f  = f . alwaysLeft
partialRight f = f . alwaysRight

total f        = always      . f
totalLeft f    = alwaysLeft  . f
totalRight f   = alwaysRight . f


instance Trivial () where
    trivial = ()

instance Trivial [a] where
    trivial = []

instance Trivial (Maybe a) where
    trivial = Nothing       

instance Trivial a => Trivial (Either a b) where
    trivial = Left trivial

instance ( Trivial a, 
           Trivial b ) => Trivial (a, b) where
    trivial = (trivial, trivial)

instance ( Trivial a,
           Trivial b,
           Trivial c ) => Trivial (a, b, c) where
    trivial = (trivial, trivial, trivial)

instance ( Trivial a,
           Trivial b,
           Trivial c,
           Trivial d ) => Trivial (a, b, c, d) where
    trivial = (trivial, trivial, trivial, trivial)

instance ( Trivial a,
           Trivial b,
           Trivial c,
           Trivial d,
           Trivial e ) => Trivial (a, b, c, d, e) where
    trivial = (trivial, trivial, trivial, trivial, trivial)


