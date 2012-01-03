

module Music.Model.MusicXML.Text 
(
  FontStyle(..)
, FontWeight(..)                       

, Font(..)
, TextDecoration
, TextDirection
, TextFormatting
, TextRotation
, GroupNameText

, Footnote

, AccidentalText
, FormattedText
, TypedText  
, Lyric
, TextElementData
, EmptyFont
, LyricFont
, LyricLanguage
, Elision
, Extend
)
where

import Music.Model.MusicXML.Base


-- *****************************************************************************
-- Simple types
-- *****************************************************************************
data FontStyle = Roman | Italic
    deriving (Show, Eq) 
data FontWeight = Regular | Bold
    deriving (Show, Eq) 


-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

data Font = Font
    { family :: [String]
    , style  :: FontStyle
    , weight :: FontWeight
    , size   :: Int }
    deriving (Show, Eq) 

type TextDecoration = TODO
type TextDirection = TODO
type TextFormatting = TODO
type TextRotation = TODO

-- | The group-name-text attribute group is used by the group-name and group-abbreviation elements.
-- The print-style and justify attribute groups are deprecated in MusicXML 2.0 in favor of the new
-- group-name-display and group-abbreviation-display elements.
type GroupNameText = TODO

                  

-- *****************************************************************************
-- Complex types
-- *****************************************************************************
               
-- | The accidental-text type represents an element with an accidental value and text-formatting
-- attributes.
type AccidentalText = TODO
{-
    <xs:complexType name="accidental-text">
        <xs:simpleContent>
            <xs:extension base="accidental-value">
                <xs:attributeGroup ref="text-formatting"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The formatted-text type represents a text element with text-formatting attributes.
type FormattedText = TODO
{-
    <xs:complexType name="formatted-text">
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="text-formatting"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}          

-- | The typed-text type represents a text element with a type attributes.
type TypedText = TODO
{-
    <xs:complexType name="typed-text">
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attribute name="type" type="xs:token"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}    
    
-- | The style-text type represents a text element with a print-style attribute group.
type StyleText = TODO
{-
    <xs:complexType name="style-text">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>
-}

-- | The lyric type represents text underlays for lyrics, based on Humdrum with support for other
--   formats. Two text elements that are not separated by an elision element are part of the same
--   syllable, but may have different text formatting. The MusicXML 2.0 XSD is more strict than the
--   2.0 DTD in enforcing this by disallowing a second syllabic element unless preceded by an
--   elision element. The lyric number indicates multiple lines, though a name can be used as well
--   (as in Finale's verse / chorus / section specification). Justification is center by default;
--   placement is below by default.

type Lyric = TODO
{-
    <xs:complexType name="lyric">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:choice>
                <xs:sequence>
                    <xs:element name="syllabic" type="syllabic" minOccurs="0"/>
                    <xs:element name="text" type="text-element-data"/>
                    <xs:sequence minOccurs="0" maxOccurs="unbounded">
                        <xs:sequence minOccurs="0">
                            <xs:element name="elision" type="elision"/>
                            <xs:element name="syllabic" type="syllabic" minOccurs="0"/>
                        </xs:sequence>
                        <xs:element name="text" type="text-element-data"/>
                    </xs:sequence>
                    <xs:element name="extend" type="extend" minOccurs="0"/>
                </xs:sequence>
                <xs:element name="extend" type="extend"/>
                <xs:element name="laughing" type="empty">
                    <xs:annotation>
                        <xs:documentation>The laughing element is taken from Humdrum.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="humming" type="empty">
                    <xs:annotation>
                        <xs:documentation>The humming element is taken from Humdrum.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:choice>
            <xs:element name="end-line" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The end-line element comes from RP-017 for Standard MIDI File Lyric meta-events. It facilitates lyric display for Karaoke and similar applications.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="end-paragraph" type="empty" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The end-paragraph element comes from RP-017 for Standard MIDI File Lyric meta-events. It facilitates lyric display for Karaoke and similar applications.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:group ref="editorial"/>
        </xs:sequence>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="justify"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

-}

-- | The text-element-data type represents a syllable or portion of a syllable for lyric text
-- underlay. A hyphen in the string content should only be used for an actual hyphenated word.
-- Language names for text elements come from ISO 639, with optional country subcodes from ISO 3166.
type TextElementData = TODO
{-
    <xs:complexType name="text-element-data">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
                <xs:attributeGroup ref="text-decoration"/>
                <xs:attributeGroup ref="text-rotation"/>
                <xs:attributeGroup ref="letter-spacing"/>
                <xs:attribute ref="xml:lang"/>
                <xs:attributeGroup ref="text-direction"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}
           

-- | The empty-font type represents an empty element with font attributes.
type EmptyFont = TODO
{-
    <xs:complexType name="empty-font">
        <xs:annotation>
        </xs:annotation>
        <xs:attributeGroup ref="font"/>
    </xs:complexType>

-}



-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- | The lyric-font type specifies the default font for a particular name and number of lyric.
type LyricFont = TODO
{-
    <xs:complexType name="lyric-font">
        <xs:annotation>
        </xs:annotation>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attributeGroup ref="font"/>
    </xs:complexType>

-}

-- | The lyric-language type specifies the default language for a particular name and number of
-- lyric.
type LyricLanguage = TODO
{-
    <xs:complexType name="lyric-language">
        <xs:annotation>
        </xs:annotation>
        <xs:attribute name="number" type="xs:NMTOKEN"/>
        <xs:attribute name="name" type="xs:token"/>
        <xs:attribute ref="xml:lang" use="required"/>
    </xs:complexType>

-}

-- | In Version 2.0, the content of the elision type is used to specify the symbol used to display
-- the elision. Common values are a no-break space (Unicode 00A0), an underscore (Unicode 005F), or
-- an undertie (Unicode 203F).
type Elision = TODO
{-
    <xs:complexType name="elision">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:string">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The extend type represents word extensions for lyrics.
type Extend = TODO
{-
    <xs:complexType name="extend">
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>
-}



-- *****************************************************************************
-- *****************************************************************************
-- *****************************************************************************



-- | The footnote element specifies editorial information that appears in footnotes in the printed
--   score. It is defined within a group due to its multiple uses within the MusicXML schema.

data Footnote = Footnote FormattedText
