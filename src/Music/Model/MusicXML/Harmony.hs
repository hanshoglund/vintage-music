

module Music.Model.MusicXML.Harmony 
where

import Music.Model.MusicXML.Base


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The harmony type is based on Humdrum's harm encoding, extended to support chord symbols in
-- popular music as well as functional harmony analysis in classical music.
--
-- If there are alternate harmonies possible, this can be specified using multiple harmony elements
-- differentiated by type. Explicit harmonies have all note present in the music; implied have some
-- notes missing but implied; alternate represents alternate analyses.
--
-- The harmony object may be used for analysis or for chord symbols. The print-object attribute
-- controls whether or not anything is printed due to the harmony element. The print-frame attribute
-- controls printing of a frame or fretboard diagram. The print-style attribute group sets the
-- default for the harmony, but individual elements can override this with their own print-style
-- values.
type Harmony = TODO
{-
    <xs:complexType name="harmony">
        <xs:sequence>
            <xs:group ref="harmony-chord" maxOccurs="unbounded"/>
            <xs:element name="frame" type="frame" minOccurs="0"/>
            <xs:element name="offset" type="offset" minOccurs="0"/>
            <xs:group ref="editorial"/>
            <xs:group ref="staff" minOccurs="0"/>
        </xs:sequence>
        <xs:attribute name="type" type="harmony-type"/>
        <xs:attributeGroup ref="print-object"/>
        <xs:attribute name="print-frame" type="yes-no"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>
-}


-- | The harp-pedals type is used to create harp pedal diagrams. The pedal-step and pedal-alter
-- elements use the same values as the step and alter elements. For easiest reading, the pedal-tuning
-- elements should follow standard harp pedal order, with pedal-step values of D, C, B, E, F, G, and
-- A.
type HarpPedals = TODO
{-
    <xs:complexType name="harp-pedals">
        <xs:sequence>
            <xs:element name="pedal-tuning" type="pedal-tuning" maxOccurs="unbounded"/>
        </xs:sequence>
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>

-}

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
