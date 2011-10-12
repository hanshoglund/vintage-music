

module Music.Model.MusicXML.Tuplet 
where

import Music.Model.MusicXML.Base



-- | The show-tuplet type indicates whether to show a part of a tuplet relating to the
-- tuplet-actual element, both the tuplet-actual and tuplet-normal elements, or neither.
data ShowTuplet = Actual | Both | NeitherTupletPart
    deriving (Show, Eq, Enum)

-- | A tuplet element is present when a tuplet is to be displayed graphically, in addition to the
-- sound data provided by the time-modification elements. The number attribute is used to distinguish
-- nested tuplets. The bracket attribute is used to indicate the presence of a bracket. If
-- unspecified, the results are implementation-dependent. The line-shape attribute is used to specify
-- whether the bracket is straight or in the older curved or slurred style. It is straight by
-- default.
--
-- Whereas a time-modification element shows how the cumulative, sounding effect of tuplets compare
-- to the written note type, the tuplet element describes how each tuplet is displayed.
--
-- The show-number attribute is used to display either the number of actual notes, the number of both
-- actual and normal notes, or neither. It is actual by default. The show-type attribute is used to
-- display either the actual type, both the actual and normal types, or neither. It is none by
-- default.
type Tuplet = TODO
{-
    <xs:complexType name="tuplet">
        <xs:sequence>
            <xs:element name="tuplet-actual" type="tuplet-portion" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The tuplet-actual element provide optional full control over how the actual part of the tuplet is displayed, including number and note type (with dots). If any of these elements are absent, their values are based on the time-modification element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="tuplet-normal" type="tuplet-portion" minOccurs="0">
                <xs:annotation>
                    <xs:documentation>The tuplet-normal element provide optional full control over how the normal part of the tuplet is displayed, including number and note type (with dots). If any of these elements are absent, their values are based on the time-modification element.</xs:documentation>
                </xs:annotation>
            </xs:element>
        </xs:sequence>
        <xs:attribute name="type" type="start-stop" use="required"/>
        <xs:attribute name="number" type="number-level"/>
        <xs:attribute name="bracket" type="yes-no"/>
        <xs:attribute name="show-number" type="show-tuplet"/>
        <xs:attribute name="show-type" type="show-tuplet"/>
        <xs:attributeGroup ref="line-shape"/>
        <xs:attributeGroup ref="position"/>
        <xs:attributeGroup ref="placement"/>
    </xs:complexType>

-}

-- | The tuplet-dot type is used to specify dotted normal tuplet types.
type TupletDot = TODO
{-
    <xs:complexType name="tuplet-dot">
        <xs:annotation>
        </xs:annotation>
        <xs:attributeGroup ref="font"/>
        <xs:attributeGroup ref="color"/>
    </xs:complexType>

    <xs:complexType name="tuplet-number">
        <xs:annotation>
            <xs:documentation>The tuplet-number type indicates the number of notes for this portion of the tuplet.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:nonNegativeInteger">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

-}

-- | The tuplet-portion type provides optional full control over tuplet specifications. It allows the
-- number and note type (including dots) to be set for the actual and normal portions of a single
-- tuplet. If any of these elements are absent, their values are based on the time-modification
-- element.
type TupletPortion = TODO
{-
    <xs:complexType name="tuplet-portion">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="tuplet-number" type="tuplet-number" minOccurs="0"/>
            <xs:element name="tuplet-type" type="tuplet-type" minOccurs="0"/>
            <xs:element name="tuplet-dot" type="tuplet-dot" minOccurs="0" maxOccurs="unbounded"/>
        </xs:sequence>
    </xs:complexType>

-}

-- | The tuplet-type type indicates the graphical note type of the notes for this portion of the
-- tuplet.
type TupletType = TODO
{-
    <xs:complexType name="tuplet-type">
        <xs:annotation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="note-type-value">
                <xs:attributeGroup ref="font"/>
                <xs:attributeGroup ref="color"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>

    <!-- Complex types derived from score.mod elements -->

-}




-- | The time-modification type represents tuplets and other durational changes.
type TimeModification = TODO
{-
    <xs:complexType name="time-modification">
        <xs:annotation>
        </xs:annotation>
        <xs:sequence>
            <xs:element name="actual-notes" type="xs:nonNegativeInteger">
                <xs:annotation>
                    <xs:documentation>The actual-notes element describes how many notes are played in the time usually occupied by the number in the normal-notes element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:element name="normal-notes" type="xs:nonNegativeInteger">
                <xs:annotation>
                    <xs:documentation>The normal-notes element describes how many notes are usually played in the time occupied by the number in the actual-notes element.</xs:documentation>
                </xs:annotation>
            </xs:element>
            <xs:sequence minOccurs="0">
                <xs:element name="normal-type" type="note-type-value">
                    <xs:annotation>
                        <xs:documentation>If the type associated with the number in the normal-notes element is different than the current note type (e.g., a quarter note within an eighth note triplet), then the normal-notes type (e.g. eighth) is specified in the normal-type and normal-dot elements.</xs:documentation>
                    </xs:annotation>
                </xs:element>
                <xs:element name="normal-dot" type="empty" minOccurs="0" maxOccurs="unbounded">
                    <xs:annotation>
                        <xs:documentation>The normal-dot element is used to specify dotted normal tuplet types.</xs:documentation>
                    </xs:annotation>
                </xs:element>
            </xs:sequence>
        </xs:sequence>
    </xs:complexType>

-}