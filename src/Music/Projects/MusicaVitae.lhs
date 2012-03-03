% For string orchestra
% Hans Höglund 2012


Introduction
==========

\begin{code}

{-# LANGUAGE 
    TypeSynonymInstances, 
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Music.Projects.MusicaVitae
(
-- * Instruments and tuning
    Part(..),
    Section(..),
    Tuning(..),
    partSection,
    sectionTuning,
    partTuning,
    ensemble,
    sectionParts,
    isViolin, isViola, isCello,
    highParts, lowParts,
    highViolinParts, highViolaParts, highCelloParts,
    lowViolinParts, lowViolaParts, lowCelloParts,
    Doubling(..),

-- * Articulation and phrasing
    Articulation(..),
    Phrasing(..) ,

-- * Playing techniques
    Str(..),
    Stopping(..),
    RightHand(..),
    LeftHand(..),
    Stopped,
    Technique,
    Cue(..),

-- * Intonation
    Intonation(..),
    intonation,

-- * Rendering
    renderCue
)
where

import Data.Convert ( convert )

import Music
import Music.Inspect
import Music.Render
import Music.Render.Midi
import Music.Time.EventList

\end{code}

\pagebreak



Preliminaries
==========

Instruments and parts
----------

The instrumentation is as follows:

  * Violin I-IV
  * Viola I-II
  * Cello I-II
  * Double Bass

A basic idea of the piece is to combine (slightly) different tunings of the instruments
using open-string techniques and harmonics. For this purpose, we will split the ensemble
into three sections, each using a different tuning:

  * Odd-numbered Vl, Vla and Vc parts tunes A4 to 443 Hz (A3 to 221.5 Hz)
  * Even-numbered Vl, Vla and Vc parts tunes A4 to 437 Hz (A3 to 218.5 Hz)
  * Double bass tunes A1 to 55 Hz

The other strings should be tuned in relation to the A-string as usual.

\begin{code}

data Part
    = Violin Int 
    | Viola  Int 
    | Cello  Int 
    | DoubleBass
    deriving ( Eq, Show )

data Section
    = High | Low | Middle
    deriving ( Eq, Show )

type Tuning = Frequency

\end{code}

We now define the relation between these types as follows:

\begin{code}

partSection   :: Part -> Section
sectionTuning :: Section -> Tuning
partTuning    :: Part -> Tuning

partSection (Violin 1) = High
partSection (Violin 2) = Low
partSection (Violin 3) = High
partSection (Violin 4) = Low
partSection (Viola 1)  = High
partSection (Viola 2)  = Low
partSection (Cello 1)  = High
partSection (Cello 2)  = Low
partSection DoubleBass = Middle

sectionTuning Low    = 434
sectionTuning Middle = 440
sectionTuning High   = 446

partTuning = sectionTuning . partSection

\end{code}

Then add some utility definitions to quickly access the various parts:

\begin{code}

ensemble                                        :: [Part]
sectionParts                                    :: Section -> [Part]

isViolin, isViola, isCello, isDoubleBass        :: Part -> Bool

highParts, lowParts                             :: [Part]
highViolinParts, highViolaParts, highCelloParts :: [Part]
lowViolinParts, lowViolaParts, lowCelloParts    :: [Part]
doubleBass                                      :: Part

ensemble
    = [ Violin 1, Violin 2, Violin 3, Violin 4
      , Viola 1, Viola 2, Cello 1, Cello 2, DoubleBass ]

sectionParts s  =  filter (\x -> partSection x == s) ensemble

highParts  =  sectionParts High
lowParts   =  sectionParts High

isViolin     (Violin _)    =  True
isViolin     _             =  False
isViola      (Viola _)     =  True
isViola      _             =  False
isCello      (Cello _)     =  True
isCello      _             =  False
isDoubleBass (DoubleBass)  =  True
isDoubleBass _             =  True

highViolinParts  =  filter isViolin (sectionParts High)
highViolaParts   =  filter isViola  (sectionParts High)
highCelloParts   =  filter isCello  (sectionParts High)
lowViolinParts   =  filter isViolin (sectionParts Low)
lowViolaParts    =  filter isViola  (sectionParts Low)
lowCelloParts    =  filter isCello  (sectionParts Low)
doubleBass       =  DoubleBass

\end{code}

All parts may be doubled. If several parts are doubled but not all, the musicians should
strive for a balance between the two main tuning sections (i.e. avoid doubling just the
upper parts or vice versa).

Certain cues are required to be played by a single musician even if the parts are
doubled, which will be marked *solo*. These passages should be distributed evenly among
the musicians, instead of being played by designated soloists.

\begin{code}

data Doubling = Solo | Tutti
    deriving ( Eq, Show )

\end{code}

\pagebreak




Pitch
----------






Articulation and dynamics
----------

\begin{code}

-- data Dynamics = PPP | PP | P | MP | MF | F | FF | FFF
--     deriving ( Show, 
--                Eq, 
--                Enum, 
--                Bounded )
-- 
-- instance Vol Dynamics where
--     volume = Volume (1e-5, 1)
-- 
-- -- short-cuts
-- 
-- ppp', pp', p', mp', mf', f', ff', fff' :: LevelFunctor a => a -> a
-- 
-- ppp' = setLevel PPP
-- pp'  = setLevel PP
-- p'   = setLevel P
-- mp'  = setLevel MP
-- mf'  = setLevel MF
-- f'   = setLevel F
-- ff'  = setLevel FF
-- fff' = setLevel FFF
-- 
-- dim :: LevelFunctor a => Accent -> Score a -> Score a
-- dim v = dynamics ((-v) *)
-- 
-- cresc :: LevelFunctor a => Accent -> Score a -> Score a
-- cresc v = dynamics (v * )


data Articulation = Articulation
    deriving ( Eq, Show )

data Phrasing 
    = NoPhrasing   
    | Phrasing { attackVel  :: Double
               , sustainVel :: [Double]
               , releaseVel :: Double
               , staccatto  :: Double }
    deriving ( Eq, Show )


\end{code}

\pagebreak




Playing techniques
----------

The piece makes use of different playing techniques in both hands. 

\begin{code}

data Str = I | II | III | IV
    deriving ( Eq, Show )

data Stopping = Open | QuarterStopped | Stopped
    deriving ( Eq, Show )

data RightHand a
    = Pizz   Articulation a
    | Single Articulation a
    | Phrase Phrasing     [a]
    | Jete   Phrasing     [a]
    deriving ( Eq, Show )

data LeftHand
    = OpenString Str

    | NaturalHarmonic       Int Str
    | NaturalHarmonicTrem   Int Int Str
    | NaturalHarmonicGliss  Int Int Str

    | QuarterStoppedString  Str

    | StoppedString         Int Str
    | StoppedStringTrem     Int Int Str
    | StoppedStringGliss    Int Int Str
    deriving ( Eq, Show )

\end{code}

As the intonation will be different between open and stopped strings, we define a 
function mapping each left-hand technique to a stopping. This stopping also distributes
over right-hand techniques (for example, an the intonation of a natural harmonic is open, 
whether played *arco* or *pizz*).

\begin{code}

class Stopped a where
    stopping :: a -> Stopping

instance Stopped LeftHand where
    stopping ( OpenString           _     ) = Open
    stopping ( NaturalHarmonic      _ _   ) = Open
    stopping ( NaturalHarmonicTrem  _ _ _ ) = Open
    stopping ( NaturalHarmonicGliss _ _ _ ) = Open
    stopping ( QuarterStoppedString _     ) = QuarterStopped
    stopping ( StoppedString        _ _   ) = Stopped
    stopping ( StoppedStringTrem    _ _ _ ) = Stopped
    stopping ( StoppedStringGliss   _ _ _ ) = Stopped

instance Stopped a => Stopped (RightHand a) where
    stopping ( Pizz   _ x )       =  stopping x
    stopping ( Single _ x )       =  stopping x
    stopping ( Phrase _ (x:xs) )  =  stopping x
    stopping ( Jete   _ (x:xs) )  =  stopping x
    
\end{code}



Cues
----------

A *cue* (sv. *insats*) is an action taken by a performer on time.

\begin{code}

type Technique = RightHand LeftHand

data Cue
    = Cue { cuePart      :: Part, 
            cueDoubling  :: Doubling, 
            cueTechnique :: Technique } 
    deriving ( Eq, Show )

\end{code}

\pagebreak




Intonation
----------

Many playing techiniques in the score calls for open strings. In this case intonation is
determined solely by the tuning.

In some cases, open-string techniques are used with an above first-position stop. This
should make the open string pitch rise about a quarter-tone step (or at least less than
a half-tone step).

Where stopped strings are used, intonation is determined by context:

 * In solo passages, intonation is individual. No attempt should be made to synchronize
   intontation (on long notes et al) for overlapping solo cues.

 * In unison passages, common intonation should be used.

\begin{code}

data Intonation
    = Tuning
    | Raised
    | Common
    | Individual
    deriving ( Eq, Show )

intonation :: Doubling -> Technique -> Intonation

intonation Tutti t = case stopping t of 
    Open           -> Tuning
    QuarterStopped -> Raised
    Stopped        -> Common

intonation Solo t = case stopping t of
    Open           -> Tuning
    QuarterStopped -> Raised
    Stopped        -> Individual

cueIntonation :: Cue -> Intonation
cueIntonation (Cue p d t) = intonation d t

raisedIntonation :: Cent
raisedIntonation = 23 Cent

\end{code}

\pagebreak




Rendering
==========

We are going to compose the piece as a score of cues. In order to hear the piece
and make musical decisions, we define a rendering function that renders a cue
to a score of Midi notes.

A caveat is that the Midi representation does not handle simultaneous tunings well.
We must therefore separete the music into different Midi channels based on part, section 
and intontation.

\begin{code}

type MidiChannel    = Int
type MidiInstrument = Maybe Int
type MidiBend       = Semitones

midiChannel :: Cue -> MidiChannel
midiChannel (Cue part doubling technique) = case (part, section, intonation') of
    ( Violin _,    High,  Tuning )     -> 0
    ( Viola  _,    High,  Tuning )     -> 1
    ( Cello  _,    High,  Tuning )     -> 2
    ( Violin _,    Low,   Tuning )     -> 3
    ( Viola  _,    Low,   Tuning )     -> 4
    ( Cello  _,    Low,   Tuning )     -> 5
    ( Violin _,    _,     Common )     -> 6
    ( Viola  _,    _,     Common )     -> 7
    ( Cello  _,    _,     Common )     -> 8
    ( DoubleBass,  _,     _      )     -> 10
    ( Violin _,    _,     Raised )     -> 11
    ( Viola _,     _,     Raised )     -> 11
    ( Cello _,     _,     Raised )     -> 13        
    -- TODO individual           
    where section     = partSection part
          intonation' = intonation doubling technique

midiInstrument :: Cue -> MidiInstrument
midiInstrument (Cue part doubling technique) = case part of
    ( Violin _ )    -> Just 40
    ( Viola _ )     -> Just 41
    ( Cello _ )     -> Just 42
    DoubleBass      -> Just 43


midiBend :: Cue -> MidiBend
midiBend (Cue part doubling technique) = case (intonation', cents') of
    ( Raised, c )       -> getCent (c + raisedIntonation) / 100
    ( Tuning, c )       -> getCent c / 100
    ( Common, c )       -> 0
    -- TODO individual
    where intonation'   = intonation doubling technique
          tuning'       = partTuning part       
          cents'        = cents tuning' - cents 440
          

renderCue :: Cue -> Score Seconds MidiNote
renderCue = undefined
-- renderCue (Cue part doubl tech) =
--     case tech of
--         Pizz x attr ->
--             note x
-- 
--         Single x attr ->
--             note dur $ Note.Note (volume $ level MF)
--                        (Pitch scale (tone $ pitch x))
--                        Nothing
-- 
--         Phrase xs attr ->
--             note dur $ Note.Note (volume $ level MF)
--                        (Pitch scale (tone 60))
--                        Nothing
-- 
--         Jete xs attr ->
--             note dur $ Note.Note (volume $ level MF)
--                        (Pitch scale (tone 60))
--                        Nothing
-- 
--     where tune = partTuning part
--           scale = makeScale tune
--           intone = intonation doubl tech
--           pitch (OpenString str) = undefined
--           pitch (NaturalHarmonic n str) = undefined
--           pitch (NaturalHarmonicTrem m n str) = undefined
--           pitch (NaturalHarmonicGliss m n str) = undefined
--           pitch (QuarterStoppedString str) = undefined
--           pitch (StoppedString p str) = p
--           pitch (StoppedStringTrem p q str) = undefined
--           pitch (StoppedStringGliss p q str) = undefined
--           makeScale = Scales.eqt 69  


\end{code}

\pagebreak




Composing the piece
==========

\begin{code}

piece :: Score Double Cue
piece = undefined

test :: Score Double MidiNote
test =             
        (before 10 . loop . stretch 0.5  $ line [ MidiNote c (Just 40) 60 0.0 30 | c <- [0..5] ])
    
    -- stretch (3) $ 
    -- chordDelay
    -- [ ( 0,      MidiNote 0 (Just 44) 72 0.0 60   )
    -- , ( 0.3,    MidiNote 0 (Just 45) 72 0.8 60   )
    -- , ( 0.5,    MidiNote 0 (Just 45) 72 0.5 60   )
    -- , ( 0.6,    MidiNote 0 (Just 45) 72 0.3 60   )
    -- , ( 1.2,      MidiNote 0 (Just 44) 72 0.8 60 ) ]
    where instr = 44

instance Render (Score Double Cue) Midi where
    render = render . (>>= renderCue)






\end{code}


