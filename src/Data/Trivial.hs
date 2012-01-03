

module Data.Trivial where
    
import Control.Monad


-- | A class for types with a trivial value.     
class Trivial a where
    trivial :: a
    
force      :: Trivial a => Maybe a -> a
forceF     :: Trivial a => (a -> b) -> Maybe a -> b
forceLeft  :: Trivial a => Either a b -> a
forceRight :: Trivial a => Either b a -> a

force (Nothing)      = trivial
force (Just x)       = x 
forceF f (Nothing)   = f trivial
forceF f (Just x)    = f x
forceLeft  (Left x)  = x
forceLeft  (Right _) = trivial
forceRight (Right x) = x
forceRight (Right _) = trivial

instance Trivial () where
    trivial = ()

instance Trivial [a] where
    trivial = []

instance Trivial (Maybe a) where
    trivial = Nothing       

-- instance (Trivial a) => Trivial (Either a b) where
--     trivial = Left trivial
