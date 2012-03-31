
module Music.Util
(
-- * Boolean
    onlyIf,
    onlyIfNot,
-- * Numbers
    negateIf,
    negateIfNot,
-- * Tuples
    mapFirst,
    mapSecond,
    duplicate,
    triplicate
)
where
         

-- | Apply the original function only if a predicate holds.
-- 
--   Intended to be used in infix form, like:
--
--   > reverse `onlyIf` (> 2 . length)
onlyIf   :: (a -> a) -> (a -> Bool) -> a -> a
onlyIf f p x = if (p x) then f x else x

-- | Apply the original function only if a predicate does not hold.
--
--   Intended to be used in infix form, like:
--
--   > reverse `onlyIfNot` (< 200 . length)
onlyIfNot :: (a -> a) -> (a -> Bool) -> a -> a
onlyIfNot f p x = if (p x) then x else f x

-- | Negate when the given predicate holds.
negateIf :: Num a => (a -> Bool) -> a -> a
negateIf = (negate `onlyIf`)

-- | Negate unless the given predicate holds.
negateIfNot :: Num a => (a -> Bool) -> a -> a
negateIfNot = (negate `onlyIfNot`)

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (x, y) = (f x, y)

mapSecond :: (a -> b) -> (c, a) -> (c, b)
mapSecond f (x, y) = (x, f y)

duplicate :: a -> (a, a)
duplicate x = (x, x)

triplicate :: a -> (a, a, a)
triplicate x = (x, x, x)   