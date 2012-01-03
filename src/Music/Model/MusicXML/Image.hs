
module Music.Model.MusicXML.Image
(
      ImageAttributes(..)
    , Image
)

where

import Music.Model.MusicXML.Base
import Music.Model.MusicXML.Layout
import Music.Model.MusicXML.Write

-- | The image-attributes group is used to include graphical images in a score. The required source
--   attribute is the URL for the image file. The required type attribute is the MIME type for the
--   image file format. Typical choices include @application\/postscript@, @image\/gif@, @image\/jpeg@,
--   @image\/png@, and @image\/tiff@.
data ImageAttributes = ImageAttributes { imageAttributesUri :: String
                                       , imageAttributesType :: String
                                       , imageAttributesPosition :: Position
                                       , imageAttributesHAlign :: HorizontalAlign
                                       , imageAttributesVAlign :: VerticalAlignImage }

-- | The image type is used to include graphical images in a score.
type Image = TODO
{-
    <xs:complexType name="image">
        <xs:attributeGroup ref="image-attributes"/>
    </xs:complexType>

    <xs:complexType name="inversion">
        <xs:annotation>
            <xs:documentation>The inversion type represents harmony inversions. The value is a number indicating which inversion is used: 0 for root position, 1 for first inversion, etc.</xs:documentation>
        </xs:annotation>
        <xs:simpleContent>
            <xs:extension base="xs:nonNegativeInteger">
                <xs:attributeGroup ref="print-style"/>
            </xs:extension>
        </xs:simpleContent>
    </xs:complexType>
-}