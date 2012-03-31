
module Music.Util
(
-- * Boolean
    ifThenElse,
    ifThenElse',        
    onlyIf,
    onlyIfNot,
-- * Numbers
    negateIf,
    negateIfNot,
-- * Tuples
    mapFirst,
    mapSecond,
    duplicate,
    triplicate,
    prod2,
    prod3
)
where

--
-- Booleans
--

-- | Function version of the if expression.
ifThenElse :: Bool -> a -> a -> a
ifThenElse p x y = if p then x else y

-- | Higher-order version of the if expression.
ifThenElse' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifThenElse' p f g x = if (p x) then f x else g x


-- | Apply the original function only if a predicate holds.
--
--   Intended to be used in infix form, like:
--
--   > reverse `onlyIf` ((>) 2 . length)
onlyIf   :: (a -> a) -> (a -> Bool) -> a -> a
onlyIf f p = ifThenElse' p f id

-- | Apply the original function only if a predicate does not hold.
--
--   Intended to be used in infix form, like:
--
--   > reverse `onlyIfNot` ((<) 200 . length)
onlyIfNot :: (a -> a) -> (a -> Bool) -> a -> a
onlyIfNot f p = ifThenElse' p id f



--
-- Numbers
--

-- | Negate when the given predicate holds.
negateIf :: Num a => (a -> Bool) -> a -> a
negateIf = (negate `onlyIf`)

-- | Negate unless the given predicate holds.
negateIfNot :: Num a => (a -> Bool) -> a -> a
negateIfNot = (negate `onlyIfNot`)


--
-- Tuples
--

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (x, y) = (f x, y)

mapSecond :: (a -> b) -> (c, a) -> (c, b)
mapSecond f (x, y) = (x, f y)

duplicate :: a -> (a, a)
duplicate x = (x, x)

triplicate :: a -> (a, a, a)
triplicate x = (x, x, x)

prod2 :: (a -> b -> c) -> (x -> y -> z) -> (a, x) -> (b, y) -> (c, z)
prod2 f g (x, y) (x', y') = (f x x', g y y')
 
prod3 :: (a -> b -> c) -> (m -> n -> o) -> (x -> y -> z) -> (a, m, x) -> (b, n, y) -> (c, o, z)
prod3 f g h (x, y, z) (x', y', z') = (f x x', g y y', h z z')


--
-- Duals
--

