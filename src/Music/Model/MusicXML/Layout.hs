

module Music.Model.MusicXML.Layout 
where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Text


-- *****************************************************************************
-- Simple types
-- *****************************************************************************

-- *****************************************************************************
-- Attribute groups
-- *****************************************************************************

-- | The directive attribute changes the default-x position of a direction. It indicates that the
-- left-hand side of the direction is aligned with the left-hand side of the time signature. If no
-- time signature is present, it is aligned with the left-hand side of the first music notational
-- element in the measure. If a default-x, justify, or halign attribute is present, it overrides the
-- directive attribute.
type Directive = Bool

-- | In cases where text extends over more than one line, horizontal alignment and justify values can
-- be different. The most typical case is for credits, such as:
-- 
--    Words and music by
--      Pat Songwriter
-- 
-- Typically this type of credit is aligned to the right, so that the position information refers to
-- the right-most part of the text. But in this example, the text is center-justified, not
-- right-justified.
-- 
-- The halign attribute is used in these situations. If it is not present, its value is the same as
-- for the justify attribute.
type HorizontalAlign = LeftCenterRight

-- | The letter-spacing attribute specifies text tracking. Values are either "normal" or a number
-- representing the number of ems to add between each letter. The number may be negative in order to
-- subtract space. The default is normal, which allows flexibility of letter-spacing for purposes of
-- text justification.
type LetterSpacing = NumberOrNormal

-- | The line-height attribute specifies text leading. Values are either "normal" or a number
-- representing the percentage of the current font height to use for leading. The default is
-- "normal". The exact normal value is implementation-dependent, but values between 100 and 120 are
-- recommended.
type LineHeight = NumberOrNormal
-- type LineShape = LineShape
-- type LineType = LineType

-- | The orientation attribute indicates whether slurs and ties are overhand (tips down) or underhand
-- (tips up). This is distinct from the placement attribute used by any notation
-- type.
type Orientation = OverUnder

-- | The placement attribute indicates whether something is above or below another element, such as a
-- note or a notation.
type Placement   = AboveBelow

-- | The position attributes are based on MuseData print suggestions. For most elements, any
-- program will compute a default x and y position. The position attributes let this be changed
-- two ways. The default-x and default-y attributes change the computation of the default
-- position. For most elements, the origin is changed relative to the left-hand side of the note
-- or the musical position within the bar (x) and the top line of the staff (y). For the following
-- elements, the default-x value changes the origin relative to the start of the current measure:
--
--     - note
--
--     - figured-bass
--
--     - harmony
--
--     - link
--
--     - directive
--
--     - measure-numbering
--
--     - all descendants of the part-list element
--
--     - all children of the direction-type element
--
-- When the part-name and part-abbreviation elements are used in the print element, the default-x
-- value changes the origin relative to the start of the first measure on the system. These values
-- are used when the current measure or a succeeding measure starts a new system. For the note,
-- figured-bass, and harmony elements, the default-x value is considered to have adjusted the
-- musical position within the bar for its descendant elements. Since the credit-words and
-- credit-image elements are not related to a measure, in these cases the default-x and default-y
-- attributes adjust the origin relative to the bottom left-hand corner of the specified page. The
-- relative-x and relative-y attributes change the position relative to the default position,
-- either as computed by the individual program, or as overridden by the default-x and default-y
-- attributes. Positive x is right, negative x is left; positive y is up, negative y is down. All
-- units are in tenths of interline space. For stems, positive relative-y lengthens a stem while
-- negative relative-y shortens it. The default-x and default-y position attributes provide
-- higher-resolution positioning data than related features such as the placement attribute and
-- the offset element. Applications reading a MusicXML file that can understand both features
-- should generally rely on the default-x and default-y attributes for their greater accuracy. For
-- the relative-x and relative-y attributes, the offset element, placement attribute, and
-- directive attribute provide context for the relative position information, so the two features
-- should be interpreted together. As elsewhere in the MusicXML format, tenths are the global
-- tenths defined by the scaling element, not the local tenths of a staff resized by the
-- staff-size element.

data Position = Position
    { defaultX  :: Tenths
    , defaultY  :: Tenths
    , relativeX :: Tenths
    , relativeY :: Tenths }
    deriving (Show, Eq)
    
-- | The printout attribute group collects the different controls over printing an object (e.g. a
-- note or rest) and its parts, including augmentation dots and lyrics. This is especially useful for
-- notes that overlap in different voices, or for chord sheets that contain lyrics and chords but no
-- melody. By default, all these attributes are set to yes. If print-object is set to no, the
-- print-dot and print-lyric attributes are interpreted to also be set to no if they are not
-- present.
data PrintStyle = PrintStyle
    { position :: Position
    , font     :: Font
    , color    :: Color }
    deriving (Show, Eq) 

-- | The printout attribute group collects the different controls over printing an object (e.g. a
-- note or rest) and its parts, including augmentation dots and lyrics. This is especially useful
-- for notes that overlap in different voices, or for chord sheets that contain lyrics and chords
-- but no melody. By default, all these attributes are set to yes. If print-object is set to no,
-- the print-dot and print-lyric attributes are interpreted to also be set to no if they are not
-- present.
data PrintOut = PrintOut
    { printObject  :: Bool
    , printDot     :: Bool
    , printSpacing :: Bool
    , printLyric   :: Bool }
    deriving (Show, Eq)     
    
-- | The group-name-text attribute group is used by the group-name and group-abbreviation elements.
-- The print-style and justify attribute groups are deprecated in MusicXML 2.0 in favor of the new
-- group-name-display and group-abbreviation-display elements.</xs:documentation>
type GroupNameText = TODO

data VerticalAlign = Top | Middle | Bottom | Baseline
    deriving (Show, Eq, Enum)

data VerticalAlignImage = ImageTop | ImageMiddle | ImageBottom
    deriving (Show, Eq, Enum)

data PrintAttributes = PrintAttributes
    { staffSpacing :: Tenths
    , newSystem    :: Bool
    , newPage      :: Bool
    , blankPage    :: Int
    , pageNumber   :: String }   
    deriving (Show, Eq) 

-- | The element and position attributes are new as of Version 2.0. They allow for bookmarks and
-- links to be positioned at higher resolution than the level of music-data elements. When no element
-- and position attributes are present, the bookmark or link element refers to the next sibling
-- element in the MusicXML file. The element attribute specifies an element type for a descendant of
-- the next sibling element that is not a link or bookmark. The position attribute specifies the
-- position of this descendant element, where the first position is 1. The position attribute is
-- ignored if the element attribute is not present. For instance, an element value of "beam" and a
-- position value of "2" defines the link or bookmark to refer to the second beam descendant of the
-- next sibling element that is not a link or bookmark. This is equivalent to an XPath test of
-- [.//beam[2]] done in the context of the sibling element.
type ElementPosition = TODO



-- | The name-display type is used for exact formatting of multi-font text in part and group names to
-- the left of the system. The print-object attribute can be used to determine what, if anything, is
-- printed at the start of each system. Enclosure for the display-text element is none by default.
-- Language for the display-text element is Italian ("it") by default.
type NameDisplay = TODO
{-
    <xs:complexType name="name-display">
        <xs:sequence>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
                <xs:element name="display-text" type="formatted-text"/>
                <xs:element name="accidental-text" type="accidental-text"/>
            </xs:choice>
        </xs:sequence>
        <xs:attributeGroup ref="print-object"/>
    </xs:complexType>
-}


-- *****************************************************************************
-- Complex types
-- *****************************************************************************

-- | The empty-print-style type represents an empty element with print-style attributes.
type EmptyPrintStyle = TODO
{-    
    <xs:complexType name="empty-print-style">
        <xs:attributeGroup ref="print-style"/>
    </xs:complexType>
-}

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
