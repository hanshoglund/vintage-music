
module Music.Model.MusicXML.Articulations 
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Sound



-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- *****************************************************************************
-- Complex types
-- *****************************************************************************
  

-- | Articulations and accents are grouped together here.
data Articulations 
    = Accent              EmptyPlacement
    | StrongAccent        EmptyPlacement
    | Staccato            EmptyPlacement
    | Tenumto             EmptyPlacement
    | DetachedLegato      EmptyPlacement
    | Staccatisimo        EmptyPlacement
    | Spiccato            EmptyPlacement
    | Scoop               EmptyLine
    | Plop                EmptyLine
    | Doit                EmptyLine
    | Falloff             EmptyLine
    | BreathMark          EmptyPlacement
    | Caesura             EmptyPlacement
    | Stress              EmptyPlacement
    | Unstress            EmptyPlacement
    | OtherArticulation   PlacementText


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
data Dynamics 
    = Dynamics
    { dynamicsType       :: DynamicsType
    , dynamicsPrintStyle :: Maybe PrintStyle
    , dynamicsPlacement  :: Maybe Placement }

data DynamicsType 
    = P
    | PP
    | PPP
    | PPPP
    | PPPPP
    | PPPPPP
    | F
    | FF
    | FFF
    | FFFF
    | FFFFF
    | FFFFFF
    | MP
    | MF
    | SF
    | SFP
    | SFPP
    | FP
    | RF
    | RFZ
    | SFZ
    | SFFZ
    | FZ
    | OtherDynamics String
    


data FermataShape 
    = NormalFermata 
    | AngledFermata 
    | SquaredFermata

-- | The fermata text content represents the shape of the fermata sign. An empty fermata element
-- represents a normal fermata. The fermata type is upright if not specified.
data Fermata 
    = Fermata
    { fermataShape      :: Maybe FermataShape
    , fermataType       :: Maybe UprightInverted
    , fermataPrintStyle :: Maybe PrintStyle }
    
-- | Wavy lines are one way to indicate trills. When used with a measure element, they should always
-- have type="continue" set.
data WavyLine ssc 
    = WavyLine
    { wavyLineType       :: StartStop
    , wavyLineNumber     :: Maybe NumberLevel
    , wavyLinePosition   :: Maybe Position
    , wavyLinePlacement  :: Maybe Placement
    , wavyLineColor      :: Maybe Color
    , wavyLineTrillSound :: Maybe TrillSound }

-- | The placement-text type represents a text element with print-style and placement attribute
-- groups.
type PlacementText = TODO
{-
    <xs:complexType name="placement-text">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-} 

-- | The harmonic type indicates natural and artificial harmonics. Allowing the type of pitch to be
-- specified, combined with controls for appearance/playback differences, allows both the notation
-- and the sound to be represented. Artificial harmonics can add a notated touching-pitch; artificial
-- pinch harmonics will usually not notate a touching pitch. The attributes for the harmonic element
-- refer to the use of the circular harmonic symbol, typically but not always used with natural
-- harmonics.
type Harmonic = TODO
{-
    <xs:complexType name="harmonic">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice minOccurs="0">
                <xs:element name="natural" type="empty">
                    <xs:annotation>
                        <xs:documentation>The natural element indicates that this is a natural harmonic. These are usually notated at base pitch rather than sounding pitch.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="artificial" type="empty">
                    <xs:annotation>
                        <xs:documentation>The artificial element indicates that this is an artificial harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:choice minOccurs="0">
                <xs:element name="base-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The base pitch is the pitch at which the string is played before touching to create the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="touching-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The touching-pitch is the pitch at which the string is touched lightly to produce the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="sounding-pitch" type="empty">
                    <xs:annotation>
                        <xs:documentation>The sounding-pitch is the pitch which is heard when playing the harmonic.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
        <xs:attributeGroup ref="print-style"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}

-- | The hammer-on and pull-off elements are used in guitar and fretted instrument notation. Since a
-- single slur can be marked over many notes, the hammer-on and pull-off elements are separate so the
-- individual pair of notes can be specified. The element content can be used to specify how the
-- hammer-on or pull-off should be notated. An empty element leaves this choice up to the
-- application.
type HammerOnPulloff = TODO
{-
    <xs:complexType name="hammer-on-pull-off">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="start-stop" use="required"/>
                <xs:attribute name="number" type="number-level" default="1"/>
                <xs:attributeGroup ref="print-style"/>
                <xs:attributeGroup ref="placement"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}


-- | The strong-accent type indicates a vertical accent mark. The type attribute indicates if the
-- point of the accent is down or up.
type StrongAccent = TODO
{-
    <xs:complexType name="strong-accent">
        <xs:annotation>
        </xs:annotation>
        <xs:complexContent>
            <xs:extension base="empty-placement">
                <xs:attribute name="type" type="up-down" default="up"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-} 

-- | The heel and toe elements are used with organ pedals. The substitution value is "no" if the
-- attribute is not present.
type HeelToe = TODO
{-
    <xs:complexType name="heel-toe">
        <xs:complexContent>
            <xs:extension base="empty-placement">
                <xs:attribute name="substitution" type="yes-no"/>
            </xs:extension>
        </xs:complexContent>
    </xs:complexType>

-}
         




                   

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
