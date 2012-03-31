
module Music.Util.Either
(
mapLeft,
mapRight,
fmapEither,
fmapLeft,
fmapRight,
)
where
    

mapLeft :: (a -> b) -> Either a c -> Either b c
mapRight :: (a -> b) -> Either c a -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft f (Right x) = Right x
mapRight f (Left x)  = Left x
mapRight f (Right x) = Right (f x)

fmapEither :: Functor f => (a -> b) -> (c -> d) -> f (Either a c) -> f (Either b d)
fmapEither f g = fmap (mapLeft f . mapRight g)

fmapLeft :: Functor f => (a -> b) -> f (Either a c) -> f (Either b c)
fmapLeft f = fmap (mapLeft f)

fmapRight :: Functor f => (a -> b) -> f (Either c a) -> f (Either c b)
fmapRight f = fmap (mapRight f)
