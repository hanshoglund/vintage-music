
{-# LANGUAGE
    FlexibleContexts #-}

-- | This module handles horizontal and vertical spacing.
module Notable.Spacing
(
    Spacing,
    standardSpacing,
    linearSpacing,
    
    spaceAll,
)

where

import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Symbols                                                             

import Data.Convert
import qualified Data.List as List

-- TODO a should not just accept NoteValues, but also absolute values for barlines, clefs etc

-- | Returns offset in spaces for a given note value.
type Spacing = NoteValue -> Spaces


-- | A standard spacing, similar to the default values in Sibelius.
standardSpacing :: Spacing
standardSpacing = convert . space . logBase 2
    where
        space x = 8 * (1.5 ** x)

-- | A linear spacing. Whole notes are twice the space of half notes etcetera. Useful in grid notation.
linearSpacing :: Spacing
linearSpacing = convert . (* 14)


-- | Given a spacing algorithm and a list of objects, return spacing for each of those objects.
--   Spacing in slower parts will be stretch to conform to the spacing of faster parts.
spaceAll :: Spacing -> [[NoteValue]] -> [[Spaces]]
spaceAll spacing xs =
    fmap (fmap spacing) xs


