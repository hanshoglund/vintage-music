
{-|
    Module      :  Music.Util
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable

    Assorted utility functions. See also the submodules:
    
      * "Music.Util.List" 
      
      * "Music.Util.Either"
      
      * "Music.Util.System"
-}

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
    absoluteDifference,
    closerThan,
    mean,

-- * Tuples  
    curry3,
    uncurry3,
    duplicate,
    triplicate,
    fst3,
    snd3,
    trd3,
    prod2,
    prod3, 
    mapFirst,
    mapSecond,
    mapPair,
    
-- * Foldable 
    removeNothingLeft,
    removeNothingRight,

    toLowerCase,
)     

where

import Data.Char( toLower )
import Data.Tuple ( swap )

import Data.Foldable ( Foldable(..) )
import qualified Data.Foldable

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

-- | Absolute difference.
absoluteDifference :: Num a => a -> a -> a
absoluteDifference x y = abs (x - y)

closerThan :: (Ord a, Num a) => a -> a -> a -> Bool
closerThan r x y = x `absoluteDifference` y < r

-- | Mean of the given values.
mean :: Fractional a => [a] -> a 
mean xs = sum xs / fromIntegral (length xs)


--
-- Tuples
--

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

-- | Map over first element. Compare "Control.Arrow.first".
mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (x, y) = (f x, y)

-- | Map over first element. Compare "Control.Arrow.second".
mapSecond :: (a -> b) -> (c, a) -> (c, b)
mapSecond f (x, y) = (x, f y)

-- | Map over first element. Compare "Control.Arrow.second".
mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (x, y) = (f x, g y)

-- | Duplicate element.
duplicate :: a -> (a, a)
duplicate x = (x, x)

-- | Triplicate element.
triplicate :: a -> (a, a, a)
triplicate x = (x, x, x)

-- | Projection of triples.
fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

-- | Projection of triples.
snd3 :: (a, b, c) -> b
snd3 (x, y, z) = y

-- | Projection of triples.
trd3 :: (a, b, c) -> c
trd3 (x, y, z) = z

-- | Lift binary functions to square.
prod2 :: (a -> b -> c) -> (x -> y -> z) -> (a, x) -> (b, y) -> (c, z)
prod2 f g (x, y) (x', y') = (f x x', g y y')
 
-- | Lift binary functions to cube.
prod3 :: (a -> b -> c) -> (m -> n -> o) -> (x -> y -> z) -> (a, m, x) -> (b, n, y) -> (c, o, z)
prod3 f g h (x, y, z) (x', y', z') = (f x x', g y y', h z z')


--
-- Foldable
-- 

removeNothingLeft :: Foldable t => t (Maybe a, b) -> [(a, b)]
removeNothingLeft = Data.Foldable.foldr rm []
    where
        rm (Nothing, b) xs  =  xs
        rm (Just x, b)  xs  =  (x, b) : xs

removeNothingRight :: Foldable t => t (a, Maybe b) -> [(a, b)]
removeNothingRight = Data.Foldable.foldr rm []
    where
        rm (a, Nothing) xs  =  xs
        rm (a, Just x)  xs  =  (a, x) : xs


toLowerCase :: String -> String
toLowerCase = map toLower
