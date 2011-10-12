

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

                   

-- *****************************************************************************
-- Element groups
-- *****************************************************************************

-- *****************************************************************************
-- Root elements
-- *****************************************************************************
