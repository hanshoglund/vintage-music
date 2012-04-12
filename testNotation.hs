
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction #-}

import Data.Convert
import Data.Ord (comparing)
import Data.Indexed
import Data.Trivial
import Numeric (showHex)

import Music
import Music.Inspect
import Music.Render.Graphics
import Music.Util
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving

import qualified Data.Foldable as Foldable

-- where can we draw articulations given a starting position?
-- articulationPoses :: StaffLines -> Direction -> NoteHeadPosition -> [NoteHeadPosition]
-- articulationPoses staffLines dir pos =
--     dropWhile (<= pos) $ poses staffLines dir pos

-- poses :: StaffLines -> Direction -> NoteHeadPosition -> [NoteHeadPosition]
-- poses l d x 
--     | isUp   d = x : if x <   l  then poses l d (x + 2) else poses l d (x + 1)  
--     | isDown d = x : if x > (-l) then poses l d (x - 2) else poses l d (x - 1)

--     where
--         r = 
-- [1,3,5,6,7,8]


-- | Amount of space of space to add to the right of vertical lines.
kVerticalLineSpace :: Double
kVerticalLineSpace = convert $ Spaces 0.3

-- | Amount of space to add to the right of accidentals.
kAccidentalSpace :: Double
kAccidentalSpace = convert $ Spaces 0.3

-- | Amount of space to add to the left of dots.
kDotSpace :: Double
kDotSpace = convert $ Spaces 0.3



mainE :: Engraving
mainE = mempty         
    <> (withBigCross $ engraveInstruction "pizz.")
   <> (withBigCross $ engraveMetronomeMark (1/2) 120)
    -- <> noteLines # scaleX 4          
    <> (withBigCross $ engraveDynamic mp `leftTo` engraveExpression "dolce")
    -- <> singleBarLine `leftTo` spaceX 0.2 `leftTo` chord3E
    <> (
         mempty
       -- <> bigCross'
       <> (             
            mempty
            -- `leftTo` vrule 2 
            `leftTo` accidentalsE 
            `leftTo` aS 
            `leftTo` chord3E 
            `leftTo` dS 
            `leftTo` dotsE 
            -- `leftTo` vrule 2
          )
       <> noteLines # scaleX 4
    )
    -- `above` allE
    -- `above` (scale 1.2 . center 6 $ minorE)
    where
        vlS = spaceX kVerticalLineSpace
        aS = spaceX kAccidentalSpace
        dS = spaceX kDotSpace

dotsE = mempty
    <> bigCross
    <> engraveDots 3 [1, -7, -12]

accidentalsE = mempty
    <> bigCross
    <> engraveAccidentals
        [
            (1, Sharp),
            -- (0, Sharp),
            (-7, Natural),
            (-12, Natural)
        ]
chord3E = mempty
    -- <> engraveRest (restFromNoteValue (1/16))
    <> drawNotes up
    where
        drawNotes stemDir = mempty
            <> engraveStem stemDir notes
            <> engraveNoteHeads stemDir notes
            <> (emptyEnvelope `withEnvelope` engraveLedgers (ledgers stemDir (map fst notes)))
            <> bigCross
        notes =
            [
                -- (2, UnfilledSquareNoteHead),
                (1, DiamondNoteHead),
                -- (0, DiamondNoteHead),
                (-7, FilledNoteHead),
                -- (-6, FilledNoteHead),
                (-12, UnfilledNoteHead)
            ]


chord2E = mempty
    <> noteLines # scaleX 10
    <> drawNotes up
    <> drawNotes down # translate (r2 (3, 0))
    where
        drawNotes stemDir = mempty
            <> engraveStem stemDir notes
            <> engraveNoteHeads stemDir notes
            <> engraveLedgers (ledgers stemDir (map fst notes))
            <> bigCross
        notes =
            [
                (2, UnfilledSquareNoteHead),
                (1, FilledSquareNoteHead),
                (0, DiamondNoteHead),
                (-7, FilledNoteHead),
                (-6, FilledNoteHead),
                (-12, UnfilledNoteHead)
            ]


minorE = mempty
    <> (alignL $ noteLines # scaleX 12)
    <> (catRight $ map (engraveNoteHead 0) $ (++ [DiamondNoteHead, CrossNoteHead, CircledCrossNoteHead, UnfilledSquareNoteHead, FilledSquareNoteHead]) $ map (noteHeadFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveRest . restFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveAccidental) $ enumFromTo minBound maxBound)
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveArticulation) $ enumFromTo minBound maxBound)



arcE = mempty
    <> noteLines # scaleX 4
    <> hc2 # rotate ((0) :: Deg) # translate (r2 (0, 1.5 * convert space))
--    <> (lw 0.05 $ hc)  # rotate ((-12) :: Deg) # translate (r2 (0, -1))
    <> engraveNote up   0    UnfilledNoteHead # translate (r2 (-1, 0))
    <> engraveNote down (-0) UnfilledNoteHead # translate (r2 (1, 0))
    where
        hc2 = caligraphy 10 $ hc
        hc = scaleY 0.34 $ lw 0.04 $ stroke $ arc (0.45 * tau :: Rad) (0.05 * tau :: Rad)

-- TODO this does not scale properly, i fear we need to render two close arcs and fill the space inbetween
caligraphy x = id
--caligraphy x = scaleX (1/x) . freeze . scaleX x


ledgersE = mempty
    <> noteLines # scaleX 4
    <> engraveClef trebleClef # translate (r2 (-2,0))
    <> engraveNote down 10 UnfilledNoteHead
    <> engraveNote up   (-9) UnfilledNoteHead
    <> engraveLedgers (ledgers up [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (         engraveClef altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveNote down 1 UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote down 2 UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote down 0 UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote up (-1) UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote up (-5) UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote up (-3) FilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote up 0 WholeNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote up 3 BrevisNoteHead
    ) # translate (r2 (-7.2, 0))


clefE = mempty
    <> noteLines # scaleX 15
    <> (
        mempty
        `leftTo` engraveClef frenchClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef trebleClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef sopranoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef mezzoSopranoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef tenorClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef baritoneClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef bassClef
        `leftTo` strutX 0.5
        `leftTo` engraveClef subBassClef
       ) # translate (r2 (-5, 0))

withBigCross = (<> bigCross)

bigCross = mempty
   -- <> bigCross'

bigCross' = (e . st) (hrule 2 <> circle 0.1 <> circle 0.2 <> vrule 2)
    where
        e  = withEnvelope emptyEnvelope
        st = lineColor darkblue

-- Instance so we can use 'draw'
instance Render Engraving Graphic where
    render = Graphic


