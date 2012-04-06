
{-|
    Module      :  Music.Util.List
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable

    This module reexports and augments "Data.List" with some new functions.
-}

module Music.Util.List
(   
    module Data.List,
    
-- * Safe versions
    maximum',
    minimum',

-- * Special searches
    filter2,
    remove2,
    partition2,
    filter3,
    remove3,
    partition3,
    reversePartition2,
    
-- * Sublists
    findSublist,

-- * Lists and tuples
    list,
    list3,
    tuple2,
    tuple3,

-- * Transformations
    cycleTimes,
    palindrome,
    palindromeInclusive,

-- * Misc  
    update,
    adjust,
    nonEmpty,
    merge,
    mergeBy,
    mergeZip,
)
where

import Data.List ( reverse )
import Data.Maybe ( fromMaybe )
import Data.Tuple ( swap )
import Music.Util ( mapFirst, mapSecond )

--
-- Safe versions
--

-- | Safe version of 'maximum'.
maximum' :: Ord a => [a] -> Maybe a
maximum' = fmap maximum . nonEmpty

-- | Safe version of 'minimum'.
minimum' :: Ord a => [a] -> Maybe a
minimum' = fmap minimum . nonEmpty


--
-- Special searches
--

-- | Extracts all consequent pairs matching the given predicate.
filter2 :: (a -> a -> Bool) -> [a] -> [(a, a)]
filter2 pred = map (fromMaybe undefined . tuple2) . fst . findSublist 2 (list pred False)

-- | Extracts all elements that are not part of a consequent pair matching the given predicate.
remove2 :: (a -> a -> Bool) -> [a] -> [a]
remove2 pred = snd . findSublist 2 (list pred False)

-- | Separate consequent pairs matching the given predicate from the other elements in the list.
partition2 :: (a -> a -> Bool) -> [a] -> ([(a, a)], [a])
partition2 pred = mapFirst (map $ fromMaybe undefined . tuple2) . findSublist 2 (list pred False)

-- | Separate consequent pairs matching the given predicate from the other elements in the list.
reversePartition2 :: (a -> a -> Bool) -> [a] -> ([(a, a)], [a])
reversePartition2 p xs = let (x, y) = partition2 (flip p) (reverse xs) in (reverse . map swap $ x, reverse y)

-- | Extracts all consequent triples matching the given predicate.
filter3 :: (a -> a -> a -> Bool) -> [a] -> [(a, a, a)]
filter3 pred = map (fromMaybe undefined . tuple3) . fst . findSublist 3 (list3 pred False)

-- | Extracts all elements that are not part of a consequent triple matching the given predicate.
remove3 :: (a -> a -> a -> Bool) -> [a] -> [a]
remove3 pred = snd . findSublist 2 (list3 pred False)

-- | Separate consequent triples matching the given predicate from the other elements in the list.
partition3 :: (a -> a -> a -> Bool) -> [a] -> ([(a, a, a)], [a])
partition3 pred = mapFirst (map $ fromMaybe undefined . tuple3) . findSublist 3 (list3 pred False)



--
-- Sublists
--

-- | Search through consecutive sublists of a length @n@.
--   The length of each sublist must be greater than zero, or the result diverges.
findSublist :: Int -> ([a] -> Bool) -> [a] -> ([[a]], [a])
findSublist n pred [] = ([], [])
findSublist n pred xs
    | pred slice  =  prependFirst slice      $ findSublist n pred (drop n xs)
    | otherwise   =  prependSecond (head xs) $ findSublist n pred (tail xs)
    where
        slice = take n xs


--
-- Lists and tuples
--

-- | Pair to list conversion.
list :: (a -> a -> b) -> b -> [a] -> b
list f z xs
    | length xs < 2  =  z
    | otherwise      =  f (xs !! 0) (xs !! 1)

-- | Triple to list conversion.
list3 :: (a -> a -> a -> b) -> b -> [a] -> b
list3 f z xs
    | length xs < 3  =  z
    | otherwise      =  f (xs !! 0) (xs !! 1) (xs !! 2)

-- | List to pair conversion.
tuple2 :: [a] -> Maybe (a, a)
tuple2 xs
    | length xs /= 2  = Nothing
    | otherwise       = Just (xs !! 0, xs !! 1)

-- | List to triple conversion.
tuple3 :: [a] -> Maybe (a, a, a)
tuple3 xs
    | length xs /= 3  = Nothing
    | otherwise       = Just (xs !! 0, xs !! 1, xs !! 2)

prependFirst :: a -> ([a], b) -> ([a], b)
prependFirst x (xs, ys) = (x:xs, ys)

prependSecond :: b -> (a, [b]) -> (a, [b])
prependSecond y (xs, ys) = (xs, y:ys)



--
-- Transformations
--

-- | Finite version of 'cycle'.
cycleTimes :: Int -> [a] -> [a]
cycleTimes n xs = take (n * length xs) $ cycle xs

-- | Non-inclusive palindrome. For example:
--
--   > palindrome [1,2,3] = [1,2,3,2]
--
palindrome :: [a] -> [a]
palindrome []        =  []
palindrome [x]       =  [x]
palindrome (x : xs)  =  x : init xs ++ reverse xs

-- | Inclusive palindrome. For example:
--
--   > palindromeInclusive [1,2,3] = [1,2,3,3,2,1]
--
palindromeInclusive :: [a] -> [a]
palindromeInclusive xs = xs ++ reverse xs



--
-- Misc
--

-- | Adjust the given index.
adjust :: Int -> (a -> a) -> [a] -> [a]
adjust i f []     = []
adjust 0 f (x:xs) = (f x):xs
adjust n f (x:xs) = x:(adjust (pred n) f xs)

-- | Update the given index.
update :: Int -> a -> [a] -> [a]
update i z []     = []
update 0 z (_:xs) = z:xs
update n z (x:xs) = x:(update (pred n) z xs)

-- | If the list is empty, return nothing, otherwise return the list.
nonEmpty :: [a] -> Maybe [a] 
nonEmpty [] = Nothing
nonEmpty xs = Just xs   


-- | Merge two sorted lists, preserving order of elements.
--
--   @merge xs ys@ is equivalent to 'sort' @(xs ++ ys)@ but with O(n) complexity.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y      = x : merge xs (y:ys)
    | otherwise  = y : merge (x:xs) ys

-- | Generic version of 'merge'.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy comp xs [] = xs
mergeBy comp [] ys = ys
mergeBy comp (x:xs) (y:ys)
    | x `comp` y == LT = x : mergeBy comp xs (y:ys)
    | otherwise        = y : mergeBy comp (x:xs) ys

-- | Zip a list with two sorted lists, preserving the order of elements in relation
--   to the natural order of the sorted lists.
--
--   In particular:
--  
--   > merge xs' ys' where (xs', ys') = mergeZip xs ys zs
--  
--   is equivalent to
--  
--   > zip (merge xs zs) ys
--
mergeZip :: Ord a => [a] -> [b] -> [a] -> ([(a, b)], [(a, b)])
mergeZip [] bs ys  =  ([], ys `zip` bs)
mergeZip xs [] ys  =  ([], [])
mergeZip xs bs []  =  (xs `zip` bs, [])
mergeZip (x:xs) (b:bs) (y:ys)
    | x < y   =  let (xbs, ybs) = mergeZip xs bs (y:ys) in ((x,b):xbs, ybs)
    | x >= y  =  let (xbs, ybs) = mergeZip (x:xs) bs ys in (xbs, (y,b):ybs)
