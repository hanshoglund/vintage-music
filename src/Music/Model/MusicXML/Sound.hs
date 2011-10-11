

module Music.Model.MusicXML.Sound 
where

import Music.Model.MusicXML.Base


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************
           
-- | The bend-sound type is used for bend and slide elements, and is
-- similar to the trill-sound attribute group. Here the beats element refers to the number of
-- discrete elements (like MIDI pitch bends) used to represent a continuous bend or slide. The
-- first-beat indicates the percentage of the direction for starting a bend; the last-beat the
-- percentage for ending it. The default choices are:
--
--      * accelerate = "no"
--
--      * beats = "4"
--
--      * first-beat = "25"
--
--      * last-beat = "75"

data BendSound = BendSound
    { accelerate    :: Bool
    , beats         :: TrillBeats
    , firstBeat     :: Percent
    , lastBeat      :: Percent }
      
-- | The trill-sound attribute group includes attributes used to guide the sound of
-- trills, mordents, turns, shakes, and wavy lines, based on MuseData sound suggestions. The default
-- choices are:
-- 
--      start-note = "upper"
-- 
--      trill-step = "whole"
-- 
--      two-note-turn = "none"
-- 
--      accelerate = "no"
-- 
--      beats = "4".
-- 
-- Second-beat and last-beat are percentages for landing on the indicated beat, with defaults of 25 and 75 respectively.
-- 
-- 
-- For mordent and inverted-mordent elements, the defaults are different:
-- 
--      * The default start-note is "main", not "upper".
-- 
--      * The default for beats is "3", not "4".
-- 
--      * The default for second-beat is "12", not "25".
-- 
--      * The default for last-beat is "24", not "75".
data TrillSound = TrillSound
    { startNote     :: StartNote
    , trillStep     :: TrillStep
    , twoNoteTurn   :: TwoNoteTurn
    -- , accelerate    :: Bool  FIXME
    -- , beats         :: TrillBeats
    , secondBeat    :: Percent
    -- , lastBeat      :: Percent 
    }
    deriving (Show, Eq)                    


-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The empty-trill-sound type represents an empty element with print-style, placement, and
-- trill-sound attributes.
type EmptyTrillSound = TODO
{-
    <xs:complexType name="empty-trill-sound">
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="trill-sound"/>
    </xs:complexType>
-}

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************