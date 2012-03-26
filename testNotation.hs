
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
import Diagrams.Prelude hiding ( Render, render, (|||), (===) )
import Diagrams.TwoD.Text ( Text )
--import Diagrams.TwoD.Types ( r2 )

infixl 6 =<=
infixl 6 =>=
infixl 6 ===

--
-- Diagrams
--

(=>=) = beside unitX
(=<=) = beside (negateV unitX)
(===) = beside (negateV unitY)


--
-- Preliminaries
--

data Notation = Notation
type Engraving = (Renderable Text b, Renderable (Path R2) b) => Diagram b R2

-- * Font representation in the engraver.
type Font   = String
type Glyph  = String
type Symbol = (Font, Glyph)

space = 0.26 

--renderNotation :: t -> Engraving
renderNotation x = mempty
    <> (moveOriginBy (r2 (0, -space/2)) noteLines)
    <> (chord1 =<= chord1 =<= chord1 =<= chord1 =<= chord1) # showOrigin

chord1 = chord1'
chord1' = mempty
    <> renderNote 3 True Brevis
    <> renderNote 1 True Whole
    <> renderNote (-1) True Unfilled
    <> renderNote (-3) True Filled
        
noteLines :: Engraving
noteLines = 
    foldr (===) mempty (replicate 5 noteLine) # moveOriginBy (r2 (0, space * (-1.5))) 
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
    translate (r2 (0, space * pos / 2)) $ 
    mempty
    <> spacer
    <> noteHead 
    -- <> noteStem
    where
        spacer     =  square 1 
                      # opacity 0

        noteHead   =  
                     text noteGlyph 
                      # font noteFont
                      # translate noteHeadOffset

        -- noteStem   =  if (hasStem nh) then noteStem' else mempty
        -- noteStem'  =  rect noteStemWidth noteStemHeight 
        --               # fc black 
        --               # moveOriginBy noteStemOffset
        
        noteStemWidth   =  0.025
        noteStemHeight  =  1

        noteHeadOffset  =  r2 $ noteHeadAdjustment nh
        noteStemOffset  =  r2 $ negateUnless dir (0.3, 0)
        
        (noteFont, noteGlyph)  =  noteSymbol nh

noteHeadAdjustment Brevis   = (0.06, 0.25)
noteHeadAdjustment Whole    = (0.058, 0.25)
noteHeadAdjustment Filled   = (0.022, 0.485)
noteHeadAdjustment Unfilled = (0.065, 0.485)

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

baseMusicFont    = "Helsinki"
specialMusicFont = "Helsinki Special"
-- baseMusicFont    = "Opus"
-- specialMusicFont = "Opus Special"

noteSymbol :: NoteHead -> Symbol
noteSymbol Unfilled  =  (specialMusicFont, "F")
noteSymbol Filled    =  (specialMusicFont, "f")
noteSymbol Whole     =  (baseMusicFont, "w")
noteSymbol Brevis    =  (baseMusicFont, "W")




--
-- Instance for drawing
--

instance Render Notation Graphic where
    render = Graphic . renderNotation
    

