{-# LANGUAGE 
    TypeFamilies,
    StandaloneDeriving,
    FlexibleContexts,  
    TypeSynonymInstances,
    UndecidableInstances #-}

module Data.Indexed
(
-- * Indexed class
Indexed(..),
Deep(..),

-- * Ranges
Range(..),
fromRange,

-- * Selections
Selection(..),
select,
selectAll,
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

-- | Data structures that can be indexed.
--
--   Minimal complete definition: 'index', 'adjust', 'firstIndex' and 'lastIndex'.
class Indexed a where
    
    -- | Type of indices.
    type Index a
    
    -- | Type of elements.
    type Elem a

    -- | Returns the element at the given index.
    index  :: Index a -> a -> Elem a

    -- | Updates the element at the given index.
    update :: Index a -> Elem a -> a -> a
    update i e = adjust i (const e)

    -- | Updates the element at the given index.
    adjust :: Index a -> (Elem a -> Elem a) -> a -> a
    
    -- | Returns the first valid index.
    firstIndex :: a -> Maybe (Index a)

    -- | Returns the last valid index.
    lastIndex :: a -> Maybe (Index a)


instance Indexed [a] where
    type Index [a] = Int
    type Elem [a] = a
    
    index  = flip (!!)
    update = List.update
    adjust = List.adjust

    firstIndex []  =  Nothing
    firstIndex _   =  Just 0

    lastIndex  []  =  Nothing
    lastIndex  x   =  Just $ length x - 1

instance Indexed (Seq a) where
    type Index (Seq a) = Int
    type Elem (Seq a) = a
    
    index  = flip Seq.index
    update = Seq.update 
    adjust = flip Seq.adjust 

    firstIndex x | Seq.null x  =  Nothing
                 | otherwise   =  Just 0
    lastIndex x  | Seq.null x  =  Nothing
                 | otherwise   =  Just $ Seq.length x - 1

instance Ord k => Indexed (Map k v) where
    type Index (Map k v) = k
    type Elem (Map k v) = v
    
    index k = maybe err id . Map.lookup k 
        where err = error "Index.index: No key"
    update = Map.insert
    adjust = flip Map.adjust

    firstIndex x | Map.null x  =  Nothing
                 | otherwise   =  Just . Set.findMin $ Map.keysSet x
    lastIndex x  | Map.null x  =  Nothing
                 | otherwise   =  Just . Set.findMax $ Map.keysSet x
    

-- | Wrapper for recursive indices.
--
--   >  index (1,2) $ Deep $ [[1,2,3], [4,5,6]]
newtype Deep c a = Deep { getDeep :: c a }
    deriving (Eq, Show, Ord, Bounded)

instance (Indexed (c a), Indexed a, Elem (c a) ~ a) => Indexed (Deep c a) where
    type Index (Deep c a) = (Index (c a), Index a)
    type Elem (Deep c a) = Elem a

    index (i, j)     =  index j . index i . getDeep
    adjust (i, j) f  =  Deep . adjust i (adjust j f) . getDeep
        
    firstIndex = undefined
    lastIndex = undefined

instance (Indexed a, Indexed b) => Indexed (a, b) where
    type Index (a, b) = Either (Index a) (Index b)
    type Elem (a, b) = Either (Elem a) (Elem b)
    
    index (Left i)  (x, _)  =  Left  (index i x)
    index (Right i) (_, x)  =  Right (index i x)

    adjust (Left i)  f (x, y)  =  (adjust i (getLeft . f . Left) x, y) 
    adjust (Right i) f (x, y)  =  (x, adjust i (getRight . f . Right) y) 
      
    firstIndex (x, y) = 
        case firstIndex x of
            Just x' -> Just (Left x')
            Nothing -> case firstIndex y of
                Just y' -> Just (Right y')
                Nothing -> Nothing

    lastIndex (x, y) = 
        case lastIndex y of
            Just y' -> Just (Right y')
            Nothing -> case lastIndex x of
                Just x' -> Just (Left x')
                Nothing -> Nothing


newtype Range a = Range { getRange :: (Index a, Index a) }

deriving instance (Indexed a, Eq (Index a)) => Eq (Range a)
deriving instance (Indexed a, Show (Index a)) => Show (Range a)

fromRange :: (Indexed a, Enum (Index a)) => Range a -> a -> [Elem a]
fromRange (Range (i, i')) x = fmap (flip index $ x) [i..i']



newtype Selection a = Selection { getSelection :: [Index a] }

deriving instance (Indexed a, Eq (Index a)) => Eq (Selection a)
deriving instance (Indexed a, Show (Index a)) => Show (Selection a)

instance Eq (Index a) => Monoid (Selection a) where
    mempty  =  Selection []
    Selection xs `mappend` Selection ys  =  Selection (List2.union xs ys)


select :: (Indexed a, Enum (Index a)) => (Elem a -> Bool) -> a -> Selection a
select p x = Selection $ filter (\i -> p $ index i x) $ getSelection $ selectAll x


selectAll :: (Indexed a, Enum (Index a)) => a -> Selection a
selectAll x = Selection $ is (firstIndex x) (lastIndex x)
    where
        is Nothing _ = []
        is _ Nothing = []
        is (Just x) (Just y) = x `enumFromTo` y

fromSelection :: Indexed a => Selection a -> a -> [Elem a]
fromSelection (Selection is) x = fmap (flip index $ x) is

