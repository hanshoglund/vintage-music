
{-# LANGUAGE 
    TypeFamilies,
    FlexibleInstances,
    FlexibleContexts,
    MultiParamTypeClasses,
    RankNTypes,
    NoMonomorphismRestriction #-}

import Prelude hiding (reverse)
import Data.Convert

import Music                  
import Music.Inspect
import Music.Render.Graphics

import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )
import Data.Convert
import Diagrams.Prelude hiding ( Render, render )
import Diagrams.TwoD.Text ( Text )

--infixl 6 ===
infixl 6 =|=
infixl 6 ~~~
infixl 6 ~|~

--(===) = beside (negateV unitY)
(=|=) = beside unitX
(~~~) = append (negateV unitY)
(~|~) = append unitX


type Engraving = (Renderable Text b, Renderable (Path R2) b) => Diagram b R2


data Notation = Notation

-- * Font representation in the engraver.
type Font   = String
type Glyph  = String
type Symbol = (Font, Glyph)

instance Render Notation Graphic where
    render = Graphic . renderNotation
    
renderNotation :: t -> Engraving


renderNotation x = mempty
    <> noteLines
    <> renderNote 2 True Unfilled

space = 0.26 


noteLines :: Engraving
noteLines = 
    foldr (===) mempty (replicate 5 noteLine) # moveOriginBy (0, space * (-1.5)) 
        where
            noteLine  =  hrule 10 # lw 0.025 
                           <> 
                         rect 10 space # opacity 0


-- | Base unit of engraving. Equal to the space between two note lines.
type Spaces = Double

-- | Unit of half a space.
type HalfSpaces = Double

-- | Base stem direction.
--   Also used for determining placement of articulations etc. 
type Direction = Bool
upwards   = True
downwards = False


-- TODO more intelligent version for chords
-- TODO proper scaling and size of spacer

renderNote :: HalfSpaces -> Direction -> NoteHead -> Engraving
renderNote pos dir nh = 
    translate (0, space * pos / 2) $ mempty
    <> spacer
    <> noteHead 
    <> noteStem
    where
        spacer     =  square 1 
                        # opacity 0
        noteHead   =  text noteGlyph 
                        # font noteFont
        noteStem   =  if (hasStem nh) then noteStem' else mempty
        noteStem'  =  rect noteStemWidth noteStemHeight 
                        # fc black 
                        # moveOriginBy noteStemOffset
        noteStemWidth   =  0.025
        noteStemHeight  =  1
        noteStemOffset  =  negateUnless dir (0.14, 0.52)
        (noteFont, noteGlyph)  =  noteSymbol nh


-- | Negate when a boolean condition holds.
negateWhen :: Num a => Bool -> a -> a
negateWhen True  = id
negateWhen False = negate

-- | Negate unless a boolean condition holds.
negateUnless :: Num a => Bool -> a -> a
negateUnless False = id
negateUnless True  = negate




-- | Represents a note head symbol.
data NoteHead 
    = Unfilled
    | Filled 
    | Whole 
    | Brevis

-- | Whether a given notehead should be drawn with a stem or not.
hasStem :: NoteHead -> Bool
hasStem Unfilled  =  True
hasStem Filled    =  True
hasStem Whole     =  False
hasStem Brevis    =  False

noteSymbol :: NoteHead -> Symbol
noteSymbol Unfilled  =  ("Helsinki Special", "F")
noteSymbol Filled    =  ("Helsinki Special", "f")
noteSymbol Whole     =  ("Helsinki", "w")
noteSymbol Brevis    =  ("Helsinki", "W")




