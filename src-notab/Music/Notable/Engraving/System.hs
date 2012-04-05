
{-# LANGUAGE
    FlexibleContexts #-}

-- | Low-level engraving of system-level objects, including the joining left line, instrument names,
--   brackets and so on. 
--
module Music.Notable.Engraving.System
(
-- * System objects

    BarNumber,
    leftLine,
    engraveBarNumber,
    engravePartName,

-- ** Accolades
    StaffList(..),
    engraveBracket,
    engraveBrace,
    engraveStaffList,


-- * Staves
    engraveStaff,


-- * Cross-staff objects

-- ** Barlines

-- ** Beams
    Beams,
    engraveBeams,

-- *** Tremolo beams
    TremoloBeams,
    engraveTremoloBeams,

-- ** Ties
    engraveTie,

-- ** Slurs
    engraveSlur,

-- ** Tuplets
    engraveTuplet,

-- ** Text
    Instruction(..),
    engraveInstruction,


-- * Systems
    CrossStaffObject(..),
    System(..),
    engraveSystem,
)
where

import Data.Indexed

import Music.Notable.Core
import Music.Notable.Core.Symbols
import Music.Notable.Core.Diagrams
import Music.Notable.Engraving.Staff

--
-- System objects
--

leftLine :: Engraving
leftLine = undefined

type BarNumber = Int

engraveBarNumber :: BarNumber -> Engraving
engraveBarNumber = undefined

engravePartName :: String -> Engraving
engravePartName = undefined

data StaffList a
    = Staff a
    | Bracketed (StaffList a)
    | Bracced (StaffList a)

engraveBracket :: Spaces -> Engraving
engraveBracket = undefined

engraveBrace :: Spaces -> Engraving
engraveBrace = undefined

engraveStaffList  :: StaffList a -> Engraving
engraveStaffList = undefined


--
-- Systems
--

data CrossStaffObject
    = BarLine
    | Beam
    | TremoloBeam
    | Tie
    | Slur
    | TupletBracket

data System = 
    System { barNumber :: Maybe BarNumber,
             staffList :: Maybe (StaffList String),
             staves    :: [(HalfSpaces, Staff)],
             crossStaffObjects :: [([Index [Staff]], CrossStaffObject)] }

engraveSystem :: System -> Engraving
engraveSystem = undefined

