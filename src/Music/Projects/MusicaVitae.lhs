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
    doubleBass,
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
    deriving ( Eq, Show, Enum )

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
ensemble     :: [Part]
sectionParts :: Section -> [Part]

isViolin, isViola, isCello, isDoubleBass :: Part -> Bool

highParts, lowParts
    :: [Part]
highViolinParts, highViolaParts, highCelloParts
    :: [Part]
lowViolinParts, lowViolaParts, lowCelloParts
    :: [Part]
doubleBass :: Part

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


Time
----------

We will use the temporal operations form `Music.Time` for composition on both event level
and structural level. We use floating point values to represent durations.

\begin{code}
type Dur = Double

\end{code}


Pitch
----------

For simplicity, we will use Midi numbers for written pitch. Sounding pitch will of course
be rendered depending on tuning and playing technique of the given part.

String number will be represented separately using a different type (named `Str` so as not
to collide with `String`).

\begin{code}
type Pitch = Int

data Str = I | II | III | IV
    deriving ( Eq, Ord, Enum, Show )

\end{code}




Articulation and dynamics
----------

\begin{code}



data Articulation
    = Straight
    -- | Stacc Articulation
    -- | Tenuto Articulation
    -- | Accent Articulation
    deriving ( Eq, Show )

data Phrasing
    = NoPhrasing
    -- | Phrasing { attackVel  :: Double
    --            , sustainVel :: [Double]
    --            , releaseVel :: Double
    --            , staccatto  :: Double }
    deriving ( Eq, Show )


\end{code}

\pagebreak




Playing techniques
----------

The piece makes use of different playing techniques in both hands.

\begin{code}
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
    | QuarterStoppedString  Str
    | NaturalHarmonic       Pitch Str
    | NaturalHarmonicTrem   Pitch Pitch Str
    | NaturalHarmonicGliss  Pitch Pitch Str
    | StoppedString         Pitch Str
    | StoppedStringTrem     Pitch Pitch Str
    | StoppedStringGliss    Pitch Pitch Str
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

A *cue* is an action taken by a performer on time.

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
type MidiPitch      = Int
type MidiBend       = Semitones
type MidiDynamic    = Int

midiChannel :: Cue -> MidiChannel
midiChannel (Cue part doubling technique) = case (part, section, intonation') of
    ( Violin _,    High,  Tuning )  ->  0
    ( Viola  _,    High,  Tuning )  ->  1
    ( Cello  _,    High,  Tuning )  ->  2
    ( Violin _,    Low,   Tuning )  ->  3
    ( Viola  _,    Low,   Tuning )  ->  4
    ( Cello  _,    Low,   Tuning )  ->  5
    ( Violin _,    _,     Common )  ->  6
    ( Viola  _,    _,     Common )  ->  7
    ( Cello  _,    _,     Common )  ->  8
    ( DoubleBass,  _,     _      )  ->  10
    ( Violin _,    _,     Raised )  ->  11
    ( Viola _,     _,     Raised )  ->  11
    ( Cello _,     _,     Raised )  ->  13
    -- TODO individual
    where section     = partSection part
          intonation' = intonation doubling technique

midiInstrument :: Cue -> MidiInstrument
midiInstrument (Cue part doubling (Pizz _ _))  =  Just 45
midiInstrument (Cue part doubling technique)   =  case part of
    ( Violin _ )  ->  Just 40
    ( Viola _ )   ->  Just 41
    ( Cello _ )   ->  Just 42
    DoubleBass    ->  Just 43


midiBend :: Cue -> MidiBend
midiBend (Cue part doubling technique) = case (intonation', cents') of
    ( Raised, c )       -> getCent (c + raisedIntonation) / 100
    ( Tuning, c )       -> getCent c / 100
    ( Common, c )       -> 0
    -- TODO individual
    where intonation'   = intonation doubling technique
          tuning'       = partTuning part
          cents'        = cents tuning' - cents 440

openStringPitch :: Part -> Str -> MidiPitch
openStringPitch (Violin _) I    =  55
openStringPitch (Violin _) II   =  62
openStringPitch (Violin _) III  =  69
openStringPitch (Violin _) IV   =  76
openStringPitch (Viola _)  I    =  48
openStringPitch (Viola _)  II   =  55
openStringPitch (Viola _)  III  =  62
openStringPitch (Viola _)  IV   =  69
openStringPitch (Cello _)  I    =  36
openStringPitch (Cello _)  II   =  43
openStringPitch (Cello _)  III  =  50
openStringPitch (Cello _)  IV   =  57
openStringPitch DoubleBass I    =  28
openStringPitch DoubleBass II   =  33
openStringPitch DoubleBass III  =  38
openStringPitch DoubleBass IV   =  43


leftHandPitch :: Part -> LeftHand -> (MidiPitch, MidiPitch)
leftHandPitch part (OpenString            s)      =  (openStringPitch part s, 0)
leftHandPitch part (NaturalHarmonic       x s)    =  (x, 0)
leftHandPitch part (NaturalHarmonicTrem   x y s)  =  (x, y)
leftHandPitch part (NaturalHarmonicGliss  x y s)  =  (x, y)
leftHandPitch part (QuarterStoppedString  s)      =  (openStringPitch part s, 0)
leftHandPitch part (StoppedString         x s)    =  (x, 0)
leftHandPitch part (StoppedStringTrem     x y s)  =  (x, y)
leftHandPitch part (StoppedStringGliss    x y s)  =  (x, y)


renderCue :: Cue -> Score Seconds MidiNote
renderCue cue@(Cue part doubling technique) = case technique of

    Pizz art leftHand ->
        note (MidiNote ch instr (fst $ leftHandPitch part leftHand) bend dyn)

    Single art leftHand ->
        note (MidiNote ch instr (fst $ leftHandPitch part leftHand) bend dyn)     -- TODO what about trem ?

    Phrase phr leftHand ->
        concatSeq $ map (\lh -> note (MidiNote ch instr pitch bend dyn)) leftHand

    Jete phr leftHand ->
        concatSeq $ map (\lh -> note (MidiNote ch instr pitch bend dyn)) leftHand

    where ch    = midiChannel cue
          instr = midiInstrument cue
          bend  = midiBend cue
          pitch = 60
          dyn   = 60
          -- TODO handle pitch, dynamics, duration, articulation, phrasing


\end{code}

\pagebreak


High-level constructors
----------

\begin{code}
openString :: Part -> Str -> Score Dur Cue
openString part str = (note $ Cue part Tutti (Single Straight $ OpenString str))
    
\end{code}

Composing the piece
==========

\begin{code}
allOpenStrings :: Score Dur Cue
allOpenStrings = stretch (1/3) $   concatSeq [ openString DoubleBass str | str <- enumFrom I ] 
                               >>> concatSeq [ openString (instr part) str | instr <- [Cello, Viola, Violin], part <- [2,1], str <- enumFrom I]

test :: Score Dur MidiNote
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

instance Render (Score Dur Cue) Midi where
    render = render . (>>= renderCue)






\end{code}


