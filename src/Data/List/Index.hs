{-# LANGUAGE 
    TypeFamilies #-}

module Data.List.Index
(
SubList(..),

-- empty,
-- append,
-- map,
-- reverse,
-- take,
-- drop
)
where

import Data.Index
import Prelude hiding (map, reverse, take, drop)
import qualified Music.Util.List as List

newtype SubList a = SubList { getSubList :: ([a], Selection [a]) }
    deriving (Eq, Show)
-- 
-- empty :: SubList a
-- empty = SubList ([], Selection [])
-- 
-- append :: SubList a -> SubList a -> SubList a
-- SubList (xs, xss) `append` SubList (ys, yss) = 
--     SubList (xs ++ ys, Selection $ getSelection xss ++ fmap (+ length xs) (getSelection yss))
-- 
-- 
-- 
-- map :: (a -> b) -> SubList a -> SubList b
-- map f (SubList (xs, ss)) = SubList (List.map f xs, mirrorSelection ss) 
-- 
-- withSelection :: ([(a,Int)] -> [(a,Int)]) -> SubList a -> SubList a
-- withSelection f (SubList (xs, ss)) = undefined
--     where
--         is  = enumFromTo 0 (length xs - 1)
--         xs' = zip xs is
-- 
-- 
-- reverse :: SubList a -> SubList a
-- reverse = withSelection $ List.reverse
-- 
-- take :: Int -> SubList a -> SubList a
-- take n = withSelection $ List.take n
-- 
-- drop :: Int -> SubList a -> SubList a
-- drop n = withSelection $ List.drop n
-- 
-- -- takeWhile :: (a -> Bool) -> SubList a -> SubList a
-- -- takeWhile p = withSelection $ List.takeWhile p
-- 
-- -- dropWhile :: (a -> Bool) -> SubList a -> SubList a
-- -- dropWhile p = withSelection $ List.dropWhile p
-- 
-- -- filter :: (a -> Bool) -> SubList a -> SubList a
-- -- filter = undefined
-- 
-- -- remove :: (a -> Bool) -> SubList a -> SubList a
-- -- remove = undefined
-- 
-- -- partition :: (a -> Bool) -> SubList a -> (SubList a, SubList a)
-- -- partition = undefined
-- 
