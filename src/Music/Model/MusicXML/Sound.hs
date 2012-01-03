

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

-- | The sound element contains general playback parameters. They can stand alone within a
-- part/measure, or be a component element within a direction.
-- 
-- Tempo is expressed in quarter notes per minute. If 0, the sound-generating program should prompt
-- the user at the time of compiling a sound (MIDI) file.
-- 
-- Dynamics (or MIDI velocity) are expressed as a percentage of the default forte value (90 for MIDI
-- 1.0).
-- 
-- Dacapo indicates to go back to the beginning of the movement. When used it always has the value
-- "yes".
-- 
-- Segno and dalsegno are used for backwards jumps to a segno sign; coda and tocoda are used for
-- forward jumps to a coda sign. If there are multiple jumps, the value of these parameters can be
-- used to name and distinguish them. If segno or coda is used, the divisions attribute can also be
-- used to indicate the number of divisions per quarter note. Otherwise sound and MIDI generating
-- programs may have to recompute this.
-- 
-- By default, a dalsegno or dacapo attribute indicates that the jump should occur the first time
-- through, while a tocoda attribute indicates the jump should occur the second time through. The
-- time that jumps occur can be changed by using the time-only attribute.
-- 
-- Forward-repeat is used when a forward repeat sign is implied, and usually follows a bar line. When
-- used it always has the value of "yes".
-- 
-- The fine attribute follows the final note or rest in a movement with a da capo or dal segno
-- direction. If numeric, the value represents the actual duration of the final note or rest, which
-- can be ambiguous in written notation and different among parts and voices. The value may also be
-- "yes" to indicate no change to the final duration.
-- 
-- If the sound element applies only one time through a repeat, the time-only attribute indicates
-- which time to apply the sound element.
-- 
-- Pizzicato in a sound element effects all following notes. Yes indicates pizzicato, no indicates
-- arco.
-- 
-- The pan and elevation attributes are deprecated in Version 2.0. The pan and elevation elements in
-- the midi-instrument element should be used instead. The meaning of the pan and elevation
-- attributes is the same as for the pan and elevation elements. If both are present, the
-- mid-instrument elements take priority.
-- 
-- The damper-pedal, soft-pedal, and sostenuto-pedal attributes effect playback of the three common
-- piano pedals and their MIDI controller equivalents. The yes value indicates the pedal is
-- depressed; no indicates the pedal is released. A numeric value from 0 to 100 may also be used for
-- half pedaling. This value is the percentage that the pedal is depressed. A value of 0 is
-- equivalent to no, and a value of 100 is equivalent to yes.
-- 
-- MIDI instruments are changed using the midi-instrument element.
-- 
-- The offset element is used to indicate that the sound takes place offset from the current score
-- position. If the sound element is a child of a direction element, the sound offset element
-- overrides the direction offset element if both elements are present. Note that the offset reflects
-- the intended musical position for the change in sound. It should not be used to compensate for
-- latency issues in particular hardware configurations.
type Sound = TODO
{-
    <xs:complexType name="sound">
        <xs:sequence>
            <xs:element name="midi-instrument" type="midi-instrument" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="offset" type="offset" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="tempo" type="non-negative-decimal"/>
        <xs:attribute name="dynamics" type="non-negative-decimal"/>
        <xs:attribute name="dacapo" type="yes-no"/>
        <xs:attribute name="segno" type="xs:token"/>
        <xs:attribute name="dalsegno" type="xs:token"/>
        <xs:attribute name="coda" type="xs:token"/>
        <xs:attribute name="tocoda" type="xs:token"/>
        <xs:attribute name="divisions" type="divisions"/>
        <xs:attribute name="forward-repeat" type="yes-no"/>
        <xs:attribute name="fine" type="xs:token"/>
        <xs:attribute name="time-only" type="xs:token"/>
        <xs:attribute name="pizzicato" type="yes-no"/>
        <xs:attribute name="pan" type="rotation-degrees"/>
        <xs:attribute name="elevation" type="rotation-degrees"/>
        <xs:attribute name="damper-pedal" type="yes-no-number"/>
        <xs:attribute name="soft-pedal" type="yes-no-number"/>
        <xs:attribute name="sostenuto-pedal" type="yes-no-number"/>
    </xs:complexType>
-}

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************



