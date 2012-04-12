
{-# LANGUAGE
    MultiParamTypeClasses,
    TypeSynonymInstances,
    NoMonomorphismRestriction #-}

import Data.Convert
import Data.Ord (comparing)
import Data.Indexed
import Data.Trivial

import Music
import Music.Inspect
import Music.Render.Graphics
import Music.Util
import Music.Util.List

import Music.Notable.Core
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving

import qualified Data.Foldable as Foldable


-- `above` allE
-- `above` (scale 1.2 . center 6 $ minorE)

-- | Amount of space of space to add to the right of vertical lines.
kVerticalLineSpace :: Double
kVerticalLineSpace = convert $ Spaces 0.3

-- | Amount of space to add to the right of accidentals.
kAccidentalSpace :: Double
kAccidentalSpace = convert $ Spaces 0.01

-- | Amount of space to add to the left of dots.
kDotSpace :: Double
kDotSpace = convert $ Spaces 0.3


spaceX x = spaceRect x (convert space)
spaceY x = spaceRect (convert space) x

mainE :: Engraving
mainE = mempty    
    <> bigCross'
    <> (vrule 2 `leftTo` accidentalsE `leftTo` aS `leftTo` chord3E `leftTo` dS `leftTo` dotsE `leftTo` vrule 2)
    <> noteLines # scaleX 4
    where
        vlS = spaceX kVerticalLineSpace
        aS = spaceX kAccidentalSpace
        dS = spaceX kDotSpace

dotsE = mempty
    <> bigCross
    <> engraveDots 3 [-4,0,0,1,5,7]  

accidentalsE = mempty
    <> bigCross
    <> engraveAccidentals 
        [
            -- (3, Sharp),
            -- (0, Sharp),
            -- (-2, Natural),
            (-4, Natural)
        ]
chord3E = mempty
    -- <> engraveRest (restFromNoteValue (1/4))
    <> drawNotes up
    where
        drawNotes stemDir = mempty
            <> engraveStem stemDir notes
            <> engraveNoteHeads stemDir notes
            <> engraveLedgers (ledgers stemDir (map fst notes))
            <> bigCross
        notes = 
            [
                -- (2, UnfilledSquareNoteHead),
                (1, FilledSquareNoteHead),
                -- (0, DiamondNoteHead),
                (-7, FilledNoteHead),
                (-6, FilledNoteHead),
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
    <> engraveNote 0 up UnfilledNoteHead # translate (r2 (-1, 0))
    <> engraveNote (-0) down UnfilledNoteHead # translate (r2 (1, 0))
    where
        hc2 = caligraphy 10 $ hc
        hc = scaleY 0.34 $ lw 0.04 $ stroke $ arc (0.45 * tau :: Rad) (0.05 * tau :: Rad)

-- TODO this does not scale properly, i fear we need to render two close arcs and fill the space inbetween
caligraphy x = id
--caligraphy x = scaleX (1/x) . freeze . scaleX x


ledgersE = mempty
    <> noteLines # scaleX 4
    <> engraveClef trebleClef # translate (r2 (-2,0))
    <> engraveNote 10 down UnfilledNoteHead
    <> engraveNote (-9) up UnfilledNoteHead
    <> engraveLedgers (ledgers up [-9,2,3,10])



allE = rotate (10 :: Deg) clefE `above` (scale (1/4) . freeze) chordE `above` chordE `above` ledgersE

chordE = mempty
    <> noteLines # scaleX 15
    <> (         engraveClef altoClef
        `leftTo` strutX 0.5
        `leftTo` engraveNote 1 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 2 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 down UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-1) up UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-5) up UnfilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote (-3) up FilledNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 0 up WholeNoteHead
        `leftTo` strutX 1
        `leftTo` engraveNote 3 up BrevisNoteHead
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


