
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









    

chordWithTextE = mempty
    <> (mempty
        -- <> bigCross
        <> engraveInstruction "pizz."
        )
    <> (mempty
        -- <> bigCross 
        <> engraveMetronomeMark (1/2) 120
        )
    <> (mempty
        -- <> bigCross 
        <> engraveDynamic mp 
        `leftTo` engraveExpression "dolce")
    
    <> (mempty
        <> (mempty
            -- <> bigCross 
            -- `leftTo` vrule 2 
            `leftTo` accidentalsE 
            `leftTo` aS 
            `leftTo` chordE 
            `leftTo` dS 
            `leftTo` dotsE 
            -- `leftTo` vrule 2
            )
        <> noteLines # scaleX 4
        )
    where
        vlS = spaceX . convert $ Spaces 0.3
        aS = spaceX . convert $ Spaces 0.3
        dS = spaceX . convert $ Spaces 0.3

accidentalsE = mempty
    <> bigCross
    <> engraveAccidentals
        [
            (1, Sharp),
            -- (0, Sharp),
            (-7, Natural),
            (-12, Natural)
        ]     
        
chordE = mempty
    <> bigCross
    <> engraveRest (restFromNoteValue (1/2))
    -- <> drawNotes down
    where
        drawNotes stemDir = mempty
            <> engraveStem stemDir notes
            <> engraveNoteHeads stemDir notes
            <> (emptyEnvelope `withEnvelope` engraveLedgers (ledgers stemDir (map fst notes)))
        notes =
            [
                -- (2, UnfilledSquareNoteHead),
                (1, DiamondNoteHead),
                -- (0, DiamondNoteHead),
                (-7, FilledNoteHead),
                (-6, FilledNoteHead),
                (-12, UnfilledNoteHead)
            ]

dotsE = mempty
    <> bigCross
    <> engraveDots 3 [1, -7, -12]



-- chordsE = mempty
--     <> noteLines # scaleX 10
--     <> drawNotes up
--     <> drawNotes down # translate (r2 (3, 0))
--     where
--         drawNotes stemDir = mempty
--             <> engraveStem stemDir notes
--             <> engraveNoteHeads stemDir notes
--             <> engraveLedgers (ledgers stemDir (map fst notes))
--             <> bigCross
--         notes =
--             [
--                 (2, UnfilledSquareNoteHead),
--                 (1, FilledSquareNoteHead),
--                 (0, DiamondNoteHead),
--                 (-7, FilledNoteHead),
--                 (-6, FilledNoteHead),
--                 (-12, UnfilledNoteHead)
--             ]


--------------------------------------------------------------------------------

engraveSlur' :: Direction -> R2 -> R2 -> Engraving
engraveSlur' stemDir start stop = baseSlur

baseSlur = style $ strokeT tr       
    where                                           
        -- x = 1.5; y = 0.3
        x = 5; y = 0.5
        -- x = 10; y = 0.8
        a = x * 0.1
        
        penTurn   =  0.9
        penWidth  =  0.05

        -- outer edge of line
        seg   =  bezier3 (r2 (a, y)) (r2 (x - a, y)) (r2 (x, 0))

        -- right inset (left is added automatically when the trail is closed)
        seg2  =  straight (r2 (-penWidth / 2, -penWidth / 2))

        -- inner edge of line
        seg3  =  reflectX . scaleY penTurn . scaleX ((x - penWidth) / x) $ seg
        
        tr  =  fromSegments [seg, seg2, seg3]
        
        style = lineWidth 0 . fillColor black
        

legatoE = mempty
    <> bigCross
    -- <> noteLines # scaleX 4
    <> engraveSlur' up (r2(0,0)) (r2(10,10))
    <> t (cat' unitX co [engraveNote down 0 FilledNoteHead, engraveNote down 0 FilledNoteHead])
    where                                                                              
        co = with { catMethod = Distrib, sep = 5 }
        t = translate (r2(0,-0.25))

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------



symbolsE = mempty
    <> (alignL $ noteLines # scaleX 12)
    <> (catRight $ map (engraveNoteHead 0) $ (++ [DiamondNoteHead, CrossNoteHead, CircledCrossNoteHead, UnfilledSquareNoteHead, FilledSquareNoteHead]) $ map (noteHeadFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveRest . restFromNoteValue) [1,1/2,1/4,1/8,1/16])
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveAccidental) $ enumFromTo minBound maxBound)
    `leftTo` strutX 1
    `leftTo` (catRight $ map (withBigCross . engraveArticulation) $ enumFromTo minBound maxBound)


--------------------------------------------------------------------------------

ledgersE = mempty
    <> noteLines # scaleX 4
    <> engraveClef trebleClef # translate (r2 (-2,0))
    <> engraveNote down 10 UnfilledNoteHead
    <> engraveNote up   (-9) UnfilledNoteHead
    <> engraveLedgers (ledgers up [-9,2,3,10])


--------------------------------------------------------------------------------

notesE = mempty
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


clefsE = mempty
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


--------------------------------------------------------------------------------

withBigCross = (<> bigCross)

bigCross = mempty
   <> bigCross'

bigCross' = (e . st) (hrule 2 <> circle 0.1 <> circle 0.2 <> vrule 2)
    where
        e  = withEnvelope emptyEnvelope
        st = lineColor darkblue





--------------------------------------------------------------------------------

-- Instance so we can use 'draw'
instance Render Engraving Graphic where
    render = Graphic                                    