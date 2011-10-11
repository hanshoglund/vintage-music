

module Music.Model.MusicXML.Articulations 
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

-- | Dynamics can be associated either with a note or a general musical direction. To avoid
-- inconsistencies between and amongst the letter abbreviations for dynamics (what is sf vs. sfz,
-- standing alone or with a trailing dynamic that is not always piano), we use the actual letters as
-- the names of these dynamic elements. The other-dynamics element allows other dynamic marks that
-- are not covered here, but many of those should perhaps be included in a more general musical
-- direction element. Dynamics elements may also be combined to create marks not covered by a single
-- element, such as sfmp.
--
-- These letter dynamic symbols are separated from crescendo, decrescendo, and wedge indications.
-- Dynamic representation is inconsistent in scores. Many things are assumed by the composer and left
-- out, such as returns to original dynamics. Systematic representations are quite complex: for
-- example, Humdrum has at least 3 representation formats related to dynamics. The MusicXML format
-- captures what is in the score, but does not try to be optimal for analysis or synthesis of
-- dynamics.
type Dynamics = TODO
{-
    <xs:complexType name="dynamics">
        <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="p" type="empty"/>
            <xs:element name="pp" type="empty"/>
            <xs:element name="ppp" type="empty"/>
            <xs:element name="pppp" type="empty"/>
            <xs:element name="ppppp" type="empty"/>
            <xs:element name="pppppp" type="empty"/>
            <xs:element name="f" type="empty"/>
            <xs:element name="ff" type="empty"/>
            <xs:element name="fff" type="empty"/>
            <xs:element name="ffff" type="empty"/>
            <xs:element name="fffff" type="empty"/>
            <xs:element name="ffffff" type="empty"/>
            <xs:element name="mp" type="empty"/>
            <xs:element name="mf" type="empty"/>
            <xs:element name="sf" type="empty"/>
            <xs:element name="sfp" type="empty"/>
            <xs:element name="sfpp" type="empty"/>
            <xs:element name="fp" type="empty"/>
            <xs:element name="rf" type="empty"/>
            <xs:element name="rfz" type="empty"/>
            <xs:element name="sfz" type="empty"/>
            <xs:element name="sffz" type="empty"/>
            <xs:element name="fz" type="empty"/>
            <xs:element name="other-dynamics" type="xs:string"/>
        </xs:choice>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}                     

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
