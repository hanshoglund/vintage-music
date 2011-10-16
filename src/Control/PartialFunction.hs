

module Control.PartialFunction
where
                             
-- | A class for functions with a known restriction.
class PartialFunction p a b where
    defined :: p a b -> a -> Bool
    papp    :: p a b -> b -> Maybe a
    pmap    :: Functor f => p a b -> f a -> Maybe f b