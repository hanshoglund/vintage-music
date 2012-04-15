{-# LANGUAGE 
    TypeFamilies,
    StandaloneDeriving,
    FlexibleContexts,  
    TypeSynonymInstances,
    UndecidableInstances #-}

module Data.Index
(
-- * Indexed class
Indexed(..),
--Deep(..),

-- * Ranges
Range(..),
fromRange,

-- * Selections
Selection(..),
emptySelection,
select,
selectAll,
selectWhen,
fromSelection,
)
where

import Data.Monoid
import Data.Foldable
import Data.Traversable

import Data.Set ( Set )
import Data.Map ( Map )
import Data.Sequence ( Seq )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Music.Util
import Music.Util.Either
import qualified Music.Util.List as List
import qualified Data.List as List2

-- | The class of types with indices.
--
--   Minimal complete definition: 'index', 'adjust' and either 'range' or 'begin' and 'end'.
class Enum (Index a) => Indexed a where
    
    -- | Type of indices.
    type Index a
    
    -- | Type of elements.
    type Value a

    -- | Returns the element at the given index.
    index  :: Index a -> a -> Value a

    -- | Updates the element at the given index.
    update :: Index a -> Value a -> a -> a
    update i e = adjust i (const e)

    -- | Updates the element at the given index.
    adjust :: Index a -> (Value a -> Value a) -> a -> a

    -- | Returns the range of valid indices.
    range :: a -> Maybe (Range a)
    range x = case (begin x, end x) of
        (Nothing, Nothing)  -> Nothing
        (Nothing, Just x)   -> Just $ Range (x, x)
        (Just x, Nothing)   -> Just $ Range (x, x)
        (Just x, Just y)    -> Just $ Range (x, y)
    
    -- | Returns the first valid index.
    begin :: a -> Maybe (Index a)
    begin = maybe Nothing (Just . fst . getRange) . range

    -- | Returns the last valid index.
    end :: a -> Maybe (Index a)
    end = maybe Nothing (Just . snd . getRange) . range

instance Indexed [a] where
    type Index [a] = Int
    type Value [a] = a
    
    index  = flip (!!)
    update = List.update
    adjust = List.adjust

    begin []  =  Nothing
    begin _   =  Just 0

    end  []  =  Nothing
    end  x   =  Just $ length x - 1

instance Indexed (Seq a) where
    type Index (Seq a) = Int
    type Value (Seq a) = a
    
    index  = flip Seq.index
    update = Seq.update 
    adjust = flip Seq.adjust 

    begin x | Seq.null x  =  Nothing
                 | otherwise   =  Just 0
    end x  | Seq.null x  =  Nothing
                 | otherwise   =  Just $ Seq.length x - 1

instance (Ord k, Enum k) => Indexed (Map k v) where
    type Index (Map k v) = k
    type Value (Map k v) = v
    
    index k = maybe err id . Map.lookup k 
        where err = error "Index.index: No key"
    update = Map.insert
    adjust = flip Map.adjust

    begin x | Map.null x  =  Nothing
                 | otherwise   =  Just . Set.findMin $ Map.keysSet x
    end x  | Map.null x  =  Nothing
                 | otherwise   =  Just . Set.findMax $ Map.keysSet x
    

-- -- | The `Deep` type acts as a wrapper for recursive indexed data structures.
-- --
-- --   >  index (1,2) $ Deep $ [[1,2,3], [4,5,6]]
-- newtype Deep c a = Deep { getDeep :: c a }
--     deriving (Eq, Show, Ord, Bounded)
-- 
-- instance (Indexed (c a), Indexed a, Value (c a) ~ a) => Indexed (Deep c a) where
--     type Index (Deep c a) = (Index (c a), Index a)
--     type Value (Deep c a) = Value a
-- 
--     index (i, j)     =  index j . index i . getDeep
--     adjust (i, j) f  =  Deep . adjust i (adjust j f) . getDeep
--         
--     begin = undefined
--     end = undefined
-- 
-- instance (Indexed a, Indexed b) => Indexed (a, b) where
--     type Index (a, b) = Either (Index a) (Index b)
--     type Value (a, b) = Either (Value a) (Value b)
--     
--     index (Left i)  (x, _)  =  Left  (index i x)
--     index (Right i) (_, x)  =  Right (index i x)
-- 
--     adjust (Left i)  f (x, y)  =  (adjust i (getLeft . f . Left) x, y) 
--     adjust (Right i) f (x, y)  =  (x, adjust i (getRight . f . Right) y) 
--       
--     begin (x, y) = 
--         case begin x of
--             Just x' -> Just (Left x')
--             Nothing -> case begin y of
--                 Just y' -> Just (Right y')
--                 Nothing -> Nothing
-- 
--     end (x, y) = 
--         case end y of
--             Just y' -> Just (Right y')
--             Nothing -> case end x of
--                 Just x' -> Just (Left x')
--                 Nothing -> Nothing


-- | The 'Range' type represent a set of consecutive indices.
newtype Range a = Range { getRange :: (Index a, Index a) }

deriving instance (Indexed a, Eq (Index a)) => Eq (Range a)
deriving instance (Indexed a, Show (Index a)) => Show (Range a)

-- | Returns all elements in the given range.
fromRange :: Indexed a => Range a -> a -> [Value a]
fromRange (Range (l, u)) x = fmap (flip index $ x) [l..u]


-- | The 'Selection' type represent an arbitrary set of indices.
newtype Selection a = Selection { getSelection :: [Index a] }

deriving instance (Indexed a, Eq (Index a)) => Eq (Selection a)
deriving instance (Indexed a, Show (Index a)) => Show (Selection a)

instance Eq (Index a) => Monoid (Selection a) where
    mempty  =  Selection []
    Selection xs `mappend` Selection ys  =  Selection (List2.union xs ys)

-- | The empty selection.
emptySelection :: Selection a
emptySelection = Selection []

-- | Convert a range to a selection.
select :: Indexed a => Range a -> Selection a
select (Range (l, u)) = Selection [l..u]

-- | Select all elements. 
selectAll :: Indexed a => a -> Selection a
selectAll = maybe emptySelection select . range

-- | Select all elements that satisfy a predicate. 
selectWhen :: Indexed a => (Value a -> Bool) -> a -> Selection a
selectWhen p x = Selection . filter (\i -> p $ index i x) . getSelection $ selectAll x

-- | Returns the selected elements.
fromSelection :: Indexed a => Selection a -> a -> [Value a]
fromSelection (Selection is) x = fmap (flip index $ x) is

