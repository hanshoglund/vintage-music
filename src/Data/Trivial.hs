

module Data.Trivial where
    

-- | A class for types with a sensible default value.     
class Trivial a where
    trivial :: a
    always  :: Maybe a -> a
    force   :: (a -> b) -> Maybe a -> b
    forceLeft  :: Either a b -> a
    forceRight :: Either b a -> a
    
    always (Nothing)  = trivial
    always (Just x)   = x 
    force f (Nothing) = f trivial
    force f (Just x)  = f x
    forceLeft (Left x)   = x
    forceRight (Right x) = x
    forceLeft _          = trivial
    forceLeft _          = trivial

instance Trivial () where
    trivial = ()

instance Trivial [a] where
    trivial = []

instance Trivial (Maybe a) where
    trivial = Nothing       

-- instance (Trivial a) => Trivial (Either a b) where
--     trivial = Left trivial
