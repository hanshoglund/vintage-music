
{-# LANGUAGE  
    TypeFamilies,
    RankNTypes,
    MultiParamTypeClasses,
    FlexibleContexts,
    GeneralizedNewtypeDeriving #-}

-- | This module contains preliminaries for music engraving.
module Music.Notable.Core
(
-- * Core types
-- ** Space
    Spaces(..),
    HalfSpaces(..),
    space,
    halfSpace,

-- *** Intervals
    prime,
    second,
    third,
    fourth,
    fifth,
    sixth,
    seventh,
    octave,

-- *** Positioning
    moveSpacesUp,
    moveSpacesDown,
    moveSpacesLeft,
    moveSpacesRight,
    moveHalfSpacesUp,
    moveHalfSpacesDown,
    moveHalfSpacesLeft,
    moveHalfSpacesRight,
    
-- ** Time
    NoteValue,
    
-- ** Miscellaneous
    StaffLines,

    Direction(..),
    up,
    down,  
    isUp,
    isDown,
    negateIfDown,
          
-- * Notation
    Notation(..),

-- * Engraving
    Engraving,
    spaceX,
    spaceY,
    spaceRect,
    spaceRectV,
    engraveSymbol,
    engraveSymbolFloating,
    engraveSpacer,
    engraveText,
    engraveSpecialText,
)
where

import Data.Convert

import Diagrams.Backend.Cairo

import Music.Util ( negateIfNot )
import Music.Notable.Core.Diagrams
import Music.Notable.Core.Symbols

--
-- Preliminaries
--

-- | Base unit in music engraving, equal to the space between two note lines. 
--
--   Note that 'Spaces' and 'HalfSpaces' are instances of 'Num' and 'Fractional', so they can be used with
--   literals. To mix spaces and halfspaces in a single expression, use 'convert'.
--
--   > space / 2 = convert halfSpace 
--   > convert space = halfSpace * 2   
--
--
newtype Spaces = Spaces { getSpaces :: Double }
    deriving ( Eq, Show, Enum, Num, Ord, Fractional, Floating, RealFrac, Real )

-- | Unit of half a space.
--
--   Note that 'Spaces' and 'HalfSpaces' are instances of 'Num' and 'Fractional', so they can be used with
--   literals. To mix spaces and halfspaces in a single expression, use 'convert'.
--
--   > space / 2 = convert halfSpace 
--   > convert space = halfSpace * 2   
--
newtype HalfSpaces = HalfSpaces { getHalfSpaces :: Double }
    deriving ( Eq, Show, Enum, Num, Ord, Fractional, Floating, RealFrac, Real )


-- (internal) The relation between Spaces and Diagram coordinates
kSpaceDefinition = 4

instance Convert Double Spaces where
    convert x = Spaces (x * kSpaceDefinition)
    reconvert (Spaces x) = x / kSpaceDefinition

instance Convert Double HalfSpaces where
    convert x = HalfSpaces (x * (2 * kSpaceDefinition))
    reconvert (HalfSpaces x) = x / (2 * kSpaceDefinition)

instance Convert Spaces HalfSpaces where
    convert (Spaces x) = HalfSpaces (x * 2)
    reconvert (HalfSpaces x) = Spaces (x / 2)

instance Convert Spaces Double where
    convert = reconvert
    reconvert = convert

instance Convert HalfSpaces Double where
    convert = reconvert
    reconvert = convert

instance Convert HalfSpaces Spaces where
    convert = reconvert
    reconvert = convert

-- | Exactly one space.
space :: Spaces
space = 1

-- | Exactly one half-space.
halfSpace :: HalfSpaces
halfSpace = 1

-- | A prime, or 0 halfspaces.
prime :: HalfSpaces
prime = 0

-- | A second, or 1 halfspaces.
second :: HalfSpaces
second = 1

-- | A second, or 2 halfspaces.
third :: HalfSpaces
third = 2

-- | A second, or 3 halfspaces.
fourth :: HalfSpaces
fourth = 3

-- | A second, or 4 halfspaces.
fifth :: HalfSpaces
fifth = 4

-- | A second, or 5 halfspaces.
sixth :: HalfSpaces
sixth = 5

-- | A second, or 6 halfspaces.
seventh :: HalfSpaces
seventh = 6

-- | A second, or 7 halfspaces.
octave :: HalfSpaces
octave = 7

--
-- Positioning etc
--

moveSpacesUp :: (V t ~ R2, Transformable t) => Spaces -> t -> t
moveSpacesUp y = translate (r2 (0, convert y))

moveSpacesDown :: (V t ~ R2, Transformable t) => Spaces -> t -> t
moveSpacesDown y = translate $ negateV (r2 (0, convert y))

moveSpacesLeft :: (V t ~ R2, Transformable t) => Spaces -> t -> t
moveSpacesLeft x = translate $ negateV (r2 (convert x, 0))

moveSpacesRight :: (V t ~ R2, Transformable t) => Spaces -> t -> t
moveSpacesRight x = translate (r2 (convert x, 0))


moveHalfSpacesUp :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveHalfSpacesUp y = translate (r2 (0, convert y))

moveHalfSpacesDown :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveHalfSpacesDown y = translate $ negateV (r2 (0, convert y))

moveHalfSpacesLeft :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveHalfSpacesLeft x = translate $ negateV (r2 (convert x, 0))

moveHalfSpacesRight :: (V t ~ R2, Transformable t) => HalfSpaces -> t -> t
moveHalfSpacesRight x = translate (r2 (convert x, 0))



--
-- Time
--

-- | Standard note value (1 for whole note, 1/4 for quarter note and so on).
type NoteValue = Double

-- negate . floor . logBase 2
-- 0 is whole note
type NoteValueLog = Int

-- amount to add for each dot
-- scanl (\x y -> x + (0.5**y)) 0 [1..10]
-- [0.0,0.5,0.75,0.875,0.9375,0.96875,0.984375]


--
-- Misc
--

-- | Number of lines in a staff.
type StaffLines = Int


-- | Direction of note stem. 
--   Needed for calculating placement of articulations, lines etc. 
newtype Direction = Direction { getDirection :: Bool }
    deriving ( Eq, Show )

up   = Direction True
down = Direction False

isUp :: Direction -> Bool
isUp = getDirection

isDown :: Direction -> Bool
isDown = not . getDirection

negateIfDown :: Num a => Direction -> a -> a
negateIfDown dir = negateIfNot (const $ isUp dir)



-- | So far just a dummy type.
data Notation = Notation

-- | A two-dimensional vector graphic object.             
--
--   This is just a synonym for 'Diagram', so it supports all transformations and outputs offered
--   by the Diagrams API. See <http://projects.haskell.org/diagrams/manual/diagrams-manual.html>.
type Engraving = Diagram Cairo R2
--type Engraving = (Renderable Text b, Renderable (Path R2) b, Backend b R2) => Diagram b R2


-- | Creates a transparent rectangle.
--   This is useful as an alternative to 'withEnvelope' for debugging purposes.
spaceRect :: Double -> Double -> Engraving
spaceRect 0 y = strut . r2 $ (0, y)
spaceRect x 0 = strut . r2 $ (x, 0)
spaceRect x y = style $ rect x y
    where
        style = fillColor blue . opacity 0.0

spaceX :: Double -> Engraving
spaceX x = spaceRect x (convert space)

spaceY :: Double -> Engraving
spaceY x = spaceRect (convert space) x



spaceRectV :: R2 -> Engraving
spaceRectV v = spaceRect (getX v) (getY v)

engraveSymbol :: Symbol -> Engraving
engraveSymbol s = {-showOrigin $ -}engraveSymbolFloating s <> engraveSpacer s

engraveSymbolFloating :: Symbol -> Engraving
engraveSymbolFloating (font', glyph) = font font' $ baselineText glyph

engraveSpacer :: Symbol -> Engraving
engraveSpacer s = translate (symbolOffset s) $ spaceRectV (symbolSpacer s)

engraveText :: String -> Engraving
engraveText str = font textFont $ baselineText str

engraveSpecialText :: String -> Engraving
engraveSpecialText str = font specialTextFont $ baselineText str

