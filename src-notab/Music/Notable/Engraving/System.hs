
{-# LANGUAGE
    FlexibleContexts #-}

-- | This module handles engraving of system-level objects, including the joining left line, instrument names,
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
    engraveStaffList,


-- * Staves
    engraveStaves,

-- * Cross-staff objects
    CrossStaffObject(..),
    engraveCrossStaffObjects,

-- * Systems
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

type StaffList = Accolade String

engraveStaffList :: StaffList -> Engraving
engraveStaffList = undefined



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


engraveStaves :: [(HalfSpaces, Staff)] -> Engraving
engraveStaves = undefined -- TODO

engraveCrossStaffObjects :: [(HalfSpaces, Staff)] 
                         -> [([(Index [Staff], Index [SpacedObject])], CrossStaffObject)] 
                         -> Engraving
engraveCrossStaffObjects = undefined -- TODO


engraveSystem :: System -> Engraving
engraveSystem = undefined -- TODO

-- leftLine
-- engraveBarNumber
-- engraveStaffList
-- engraveStaves
-- engraveCrossStaffObjects
