
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
    Accolade(..),
    engraveBracket,
    engraveBrace,
    engraveAccolade,


-- -- * Staves
--     engraveStaff,

-- * Cross-staff objects
-- 
-- -- ** Barlines
-- 
-- -- ** Beams
--     Beams,
--     engraveBeams,
-- 
-- -- *** Tremolo beams
--     TremoloBeams,
--     engraveTremoloBeams,
-- 
-- -- ** Ties
--     engraveTie,
-- 
-- -- ** Slurs
--     engraveSlur,
-- 
-- -- ** Tuplets
--     engraveTuplet,
-- 
-- -- ** Text
--     Instruction(..),
--     engraveInstruction,


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

data Accolade a
    = NoAccolade a
    | Bracketed (Accolade a)
    | Bracced (Accolade a)

engraveBracket :: Spaces -> Engraving
engraveBracket = undefined

engraveBrace :: Spaces -> Engraving
engraveBrace = undefined

engraveAccolade  :: Accolade a -> Engraving
engraveAccolade = undefined


--
-- Systems
--

data CrossStaffObject
    = CrossStaffBarLine
    | CrossStaffBeam
    | CrossStaffTremoloBeam
    | CrossStaffTie
    | CrossStaffSlur
    | CrossStaffTupletBracket

data System = 
    System { barNumber :: Maybe BarNumber,
             staffList :: Maybe (Accolade String),
             staves    :: [(HalfSpaces, Staff)],
             crossStaffObjects :: [([(Index [Staff], Index [SpacedObject])], CrossStaffObject)] }

engraveSystem :: System -> Engraving
engraveSystem = undefined

