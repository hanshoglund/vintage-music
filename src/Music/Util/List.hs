
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

-- * Searching and extracting
-- ** Filters
    filter,
    remove,
    partition,
    filter2,
    remove2,
    partition2,
    filter3,
    remove3,
    partition3,

-- ** Reverse filters
    reversePartition2,
    reversePartition3,

-- ** Sublists
    filterSublists,
    removeSublists,
    partitionSublists,

-- * Lists and tuples
    list,
    list3,
    tuple2,
    tuple3,

-- * Transformations
    spin,
    palindrome,
    palindromeInclusive,
    mapCollect,
    concatMapAccumL,
    removeDuplicates,
    removeEquals,

-- * Indexing
    divide,
    update,
    adjust,
    insertIndex,
    insertIndexBy,
    nonEmpty,  
    ifNonEmpty,
    oneOf,

-- * Ordered lists
-- ** Safe min and max
    maximum',
    minimum',
    maximumWith,
    minimumWith,

-- ** Descending lists
    invertOrdering,
    reverseSort,
    reverseSortBy,

-- ** Merging
    merge,
    mergeBy,
    mergeZip,
    mergeZipBy,
)
where

import Data.List
import Data.Maybe ( fromMaybe, catMaybes )
import Data.Tuple ( swap )
import qualified Data.Set as Set

import Music.Util ( mapFirst, mapSecond, uncurry3 )




--
-- Safe versions
--

-- | Safe version of 'maximum'.
maximum' :: Ord a => [a] -> Maybe a
maximum' = fmap maximum . nonEmpty

-- | Safe version of 'minimum'.
minimum' :: Ord a => [a] -> Maybe a
minimum' = fmap minimum . nonEmpty

-- | A version of 'maximum' with an optional null element.
maximumWith :: Ord a => a -> [a] -> a
maximumWith z = maybe z id . maximum'

-- | A version of 'minimum' with an optional null element.
minimumWith :: Ord a => a -> [a] -> a
minimumWith z = maybe z id . minimum'


--
-- Special searches
--

-- | `remove` takes a predicate and a list and returns the lists of elements which do not satisfy the predicate, i.e.
--
--   > remove p xs = [ x | x <- xs, (not . p) x ]
remove :: (a -> Bool) -> [a] -> [a]
remove f = snd . partition f

-- | `filter` lifted to consecutive pairs.
filter2 :: (a -> a -> Bool) -> [a] -> [(a, a)]
filter2 pred = map unsafeTuple2 . fst . psl2 pred

-- | `remove` lifted to consecutive pairs.
remove2 :: (a -> a -> Bool) -> [a] -> [a]
remove2 pred = snd . psl2 pred

-- | `partition` lifted to consecutive pairs.
partition2 :: (a -> a -> Bool) -> [a] -> ([(a, a)], [a])
partition2 pred = mapFirst (map unsafeTuple2) . psl2 pred

-- | `filter` lifted to consecutive triples.
filter3 :: (a -> a -> a -> Bool) -> [a] -> [(a, a, a)]
filter3 pred = map unsafeTuple3 . fst . psl3 pred

-- | `remove` lifted to consecutive triples.
remove3 :: (a -> a -> a -> Bool) -> [a] -> [a]
remove3 pred = snd . psl3 pred

-- | `partition` lifted to consecutive triples.
partition3 :: (a -> a -> a -> Bool) -> [a] -> ([(a, a, a)], [a])
partition3 pred = mapFirst (map unsafeTuple3) . psl3 pred

psl2 pred = partitionSublists 2 (list pred False)
psl3 pred = partitionSublists 3 (list3 pred False)
unsafeTuple2 = fromMaybe undefined . tuple2
unsafeTuple3 = fromMaybe undefined . tuple3


-- | `partition` lifted to consecutive pairs, searching in reverse order.
reversePartition2 :: (a -> a -> Bool) -> [a] -> ([(a, a)], [a])
reversePartition2 p xs = (reverse . map swap $ x, reverse y)
    where
        (x, y) = partition2 (flip p) (reverse xs)

-- | `partition` lifted to consecutive triples, searching in reverse order.
reversePartition3 :: (a -> a -> a -> Bool) -> [a] -> ([(a, a, a)], [a])
reversePartition3 p xs = (reverse . map swap3 $ x, reverse y)
    where
        (x, y) = partition3 (flip3 p) (reverse xs)
        swap3 (x, y, z) = (z, y, x)
        flip3 f x y z = f z y x

-- TODO add these?
select2 :: Ord a => (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a]
select2 f g xs = fmap g' as `merge` bs
    where
        (as, bs)  =  partition2 f xs
        g'        =  uncurry g

select3 :: Ord a => (a -> a -> a -> Bool) -> (a -> a -> a -> a) -> [a] -> [a]
select3 f g xs = fmap g' as `merge` bs
    where
        (as, bs)  =  partition3 f xs
        g'        =  uncurry3 g


--
-- Sublists
--

filterSublists :: Int -> ([a] -> Bool) -> [a] -> [[a]]
filterSublists n pred = fst . partitionSublists n pred

removeSublists :: Int -> ([a] -> Bool) -> [a] -> [a]
removeSublists n pred = snd . partitionSublists n pred


-- | Extract consecutive sublists of a length @n@.
--   The length of each sublist must be greater than zero, or the result diverges for non-empty lists.
--
--   `partitionSublists` is greedy, so
--
--   > partitionSublists 3 (isPrefixOf "ha") "haha" ==> (["hah"],"a")
--   > partitionSublists 2 (isPrefixOf "ha") "haha" ==> (["ha","ha"],"")
--
partitionSublists :: Int -> ([a] -> Bool) -> [a] -> ([[a]], [a])
partitionSublists n pred [] = ([], [])
partitionSublists n pred xs
    | pred as    =  as `prependFirst`  partitionSublists n pred bs
    | otherwise  =  a  `prependSecond` partitionSublists n pred b
    where
        as = take n xs
        bs = drop n xs
        a  = head xs
        b  = tail xs


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
spin :: Int -> [a] -> [a]
spin n xs = take (n * length xs) $ cycle xs

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

mapCollect :: (a -> (b, Maybe c)) -> [a] -> ([b], [c])
mapCollect f xs = (bs, catMaybes cs)
    where
        (bs, cs) = unzip $ map f xs

-- | Combination of 'concatMap' and 'mapAccumL'.
concatMapAccumL :: (acc -> x -> (acc, [y])) -> acc -> [x] -> (acc, [y])
concatMapAccumL _ s []     = (s,  [])
concatMapAccumL f s (x:xs) = (s'', y ++ ys)
    where 
        (s',  y ) = f s x
        (s'', ys) = concatMapAccumL f s' xs   
        

-- | Return a list containing the same set of elements as the given list.
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = Set.toList . Set.fromList

-- | Remove consequent equal pairs.
removeEquals :: Eq a => [a] -> [a]
removeEquals = remove2 (==)


--
-- Indexing
--

-- | Divide into smaller parts.
divide :: Int -> [a] -> [[a]]
divide n [] = []
divide n xs = take n xs : divide n (drop n xs)

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

insertIndex :: Ord a => a -> [a] -> Int
insertIndex = insertIndexBy compare

insertIndexBy :: (a -> a -> Ordering) -> a -> [a] -> Int
insertIndexBy = ix 0
    where
        ix n _   _ [] = n
        ix n cmp x (y:ys)
            | x `cmp` y == GT  =  ix (n + 1) cmp x ys
            | otherwise        =  n         

--
-- Misc
--

-- | If the list is empty, return nothing, otherwise return the list.
nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs      

ifNonEmpty :: ([a] -> b) -> b -> [a] -> b
ifNonEmpty f z = maybe z f . nonEmpty

oneOf :: Eq a => [a] -> a -> Bool
oneOf = flip elem


-- | Inverts an ordering, i.e.
--
--   > LT -> GT
--   > EQ -> EQ
--   > GT -> LT
invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

-- | Equivalent to @reverse . sort@, but more efficient.
reverseSort :: Ord a => [a] -> [a]
reverseSort = reverseSortBy compare

-- | Equivalent to @reverse . sortBy comp@, but more efficient.
reverseSortBy :: (a -> a -> Ordering) -> [a] -> [a]
reverseSortBy comp = sortBy (\x y -> invertOrdering $ x `comp` y)



-- | Merge two sorted lists, preserving order of elements.
--
--   @merge xs ys@ is equivalent to 'sort' @(xs ++ ys)@ but with O(n) complexity.
merge :: Ord a => [a] -> [a] -> [a]
merge = mergeBy compare

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

mergeZipBy :: (a -> a -> Ordering) -> [a] -> [b] -> [a] -> ([(a, b)], [(a, b)])
mergeZipBy comp [] bs ys  =  ([], ys `zip` bs)
mergeZipBy comp xs [] ys  =  ([], [])
mergeZipBy comp xs bs []  =  (xs `zip` bs, [])
mergeZipBy comp (x:xs) (b:bs) (y:ys)
    | x `comp` y == LT  =  let (xbs, ybs) = mergeZipBy comp xs bs (y:ys) in ((x,b):xbs, ybs)
    | otherwise         =  let (xbs, ybs) = mergeZipBy comp (x:xs) bs ys in (xbs, (y,b):ybs)

