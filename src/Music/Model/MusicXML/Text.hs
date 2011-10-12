

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

, AccidentalText
, FormattedText
, TypedText
)
where

import Music.Model.MusicXML.Base


-- *****************************************************************************
-- Simple types
-- *****************************************************************************
data FontStyle = NormalFontStyle | ItalicFontStyle
    deriving (Show, Eq) 
data FontWeight = NormalFontWeight | ItalicFontWeight
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



-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************