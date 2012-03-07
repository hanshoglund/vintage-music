
module Music.Util.Either
where
    

mapL :: (a -> b) -> Either a c -> Either b c
mapR :: (a -> b) -> Either c a -> Either c b
mapL f (Left x)  = Left (f x)
mapL f (Right x) = Right x
mapR f (Left x)  = Left x
mapR f (Right x) = Right (f x)

fmapE :: Functor f => (a -> b) -> (c -> d) -> f (Either a c) -> f (Either b d)
fmapE f g = fmap (mapL f . mapR g)

fmapL :: Functor f => (a -> b) -> f (Either a c) -> f (Either b c)
fmapL f = fmap (mapL f)

fmapR :: Functor f => (a -> b) -> f (Either c a) -> f (Either c b)
fmapR f = fmap (mapR f)
