% Passager, for string orchestra
% Hans Höglund 2012


Introduction
==========

This document describes a piece for string orchestra. It is a literate program, which may
be read as well as executed.

To convert into a readable Pdf file, install Pandoc and use

    markdown2pdf -o MusicaVitae.pdf MusicaVitae.lhs

To run the program, install the Haskell platform and the `Music.Time` module. For correct
playback of intonation, use a sampler that support Midi Tuning Standard, such as
[Timidity](http://timidity.sourceforge.net/).


\begin{code}

{-# LANGUAGE
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses #-}

module Music.Projects.MusicaVitae
(
-- * Preliminaries
-- ** Instruments and tuning
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

-- ** Time and pitch
    Dur(..),
    Pitch(..),
    Str(..),

-- ** Dynamics
    Level(..),
    Dynamics(..),
    ppp, pp, p, mf, f, ff, fff,
    cresc, 
    dim,

-- ** Articulation
    Articulation(..),
    Phrasing(..),
    staccato,
    tenuto,
    legato,
    portato,

-- ** Playing techniques
    RightHand(..),
    LeftHand(..),
    Stopping(..),
    Stopped,
    Technique,

-- ** Intonation
    Intonation(..),
    intonation,
    cueIntonation,
    raisedIntonation,

-- ** Cues
    Cue(..),
    mapCuePart,
    mapCueDoubling,
    mapCueDynamics,
    mapCueTechnique,
    setCuePart,
    setCueDoubling,
    setCueDynamics,
    setCueTechnique,

-- * Rendering
    renderCue,

-- * High-level composition
-- ** Open strings
    openString,
    openStringPizz,
    openStringJete,
    openStrings,

-- ** Natural harmonics
    naturalHarmonic,
    naturalHarmonicPizz,
    naturalHarmonicJete,
    naturalHarmonics,

-- ** Quarter stopped strings
    quarterStoppedString,
    quarterStoppedStrings,

-- ** Stopped strings
    stoppedString,
    stoppedStringPizz,
    stoppedStringJete,
    stoppedStrings,
)
where

import Prelude hiding ( reverse )

import Data.Convert ( convert )
import qualified Data.List as List

import Music
import Music.Time.Tremolo
import Music.Time.Functors
import Music.Render
import Music.Render.Midi
import Music.Inspect
import Music.Util.List
import Music.Util.Either

\end{code}

\pagebreak



Preliminaries
==========

In this chapter we will describe the musical preliminaries needed to compose the piece.

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




Time and pitch
----------

We will use the temporal operations form `Music.Time` for composition on both event level
and structural level. We use floating point values to represent durations.

\begin{code}
type Dur = Double

\end{code}

For simplicity, we will use Midi numbers for written pitch. Sounding pitch will of course
be rendered depending on tuning and playing technique of the given part.

String number will be represented separately using a different type (named `Str` so as not
to collide with `String`).

\begin{code}
type Pitch = Int

data Str = I | II | III | IV
    deriving ( Eq, Ord, Enum, Show )

\end{code}

\pagebreak




Dynamics
----------

We use a simple linear representation for dynamics. Level 0 corresponds to some medium level
dynamic, level 1 to extremely loud and level -1 to extremely soft. A dynamic is a function
from time to level, generalizing crescendo, diminuendo and so on.

\begin{code}
type Level = Double

newtype Dynamics = Dynamics { getDynamics :: Dur -> Level }
    deriving ( Eq )

instance Show Dynamics where
    show x = ""

ppp, pp, p, mf, f, ff, fff :: Dynamics
ppp = Dynamics $ const (-0.8)
pp  = Dynamics $ const (-0.6)
p   = Dynamics $ const (-0.3)
mf  = Dynamics $ const 0
f   = Dynamics $ const 0.25
ff  = Dynamics $ const 0.5
fff = Dynamics $ const 0.7

cresc, dim :: Dynamics
cresc = Dynamics id
dim   = Dynamics (succ . negate)


instance Num Dynamics where
    (Dynamics x) + (Dynamics y)  =  Dynamics (\t -> x t + y t)
    (Dynamics x) * (Dynamics y)  =  Dynamics (\t -> x t * y t)
    signum (Dynamics x)          =  Dynamics (signum . x)
    abs (Dynamics x)             =  Dynamics (abs . x)
    fromInteger n                =  Dynamics (const $ fromInteger n)

\end{code}

\pagebreak




Articulation
----------

\begin{code}
data Articulation
    = Straight
    | Accent   Double Articulation
    | Duration Double Articulation
    deriving ( Eq, Show )

data Phrasing
    = Phrasing
    | Binding Double Phrasing
    | Begin Articulation Phrasing
    | End Articulation Phrasing
    deriving ( Eq, Show )

staccato :: Articulation -> Articulation
staccato = Duration 0.8

tenuto :: Articulation -> Articulation
tenuto = Duration 1.2

legato :: Phrasing -> Phrasing
legato = Binding 1.2

portato :: Phrasing -> Phrasing
portato = Binding 0.8


\end{code}

\pagebreak




Playing techniques
----------

The piece makes use of different playing techniques in both hands.

The `RightHand` type is parameterized over time, articulation, phrasing and content.
The `LeftHand` type is parameterized over pitch and string.

\begin{code}
data RightHand t c r a
    = Pizz   c a
    | Single c a
    | Phrase r [(t, a)]
    | Jete   r [a]
    deriving ( Eq, Show )

data LeftHand p s
    = OpenString            s
    | NaturalHarmonic       p s
    | NaturalHarmonicTrem   p p s
    | NaturalHarmonicGliss  p p s
    | QuarterStoppedString  s
    | StoppedString         p s
    | StoppedStringTrem     p p s
    | StoppedStringGliss    p p s
    deriving ( Eq, Show )


\end{code}

As the intonation will be different between open and stopped strings, we define a
function mapping each left-hand technique to a stopping. This stopping also distributes
over right-hand techniques (for example, an the intonation of a natural harmonic is open,
whether played *arco* or *pizz*).

\begin{code}
data Stopping = Open | QuarterStopped | Stopped
    deriving ( Eq, Show )

class Stopped a where
    stopping :: a -> Stopping

instance Stopped (LeftHand p s) where
    stopping ( OpenString           _     ) = Open
    stopping ( NaturalHarmonic      _ _   ) = Open
    stopping ( NaturalHarmonicTrem  _ _ _ ) = Open
    stopping ( NaturalHarmonicGliss _ _ _ ) = Open
    stopping ( QuarterStoppedString _     ) = QuarterStopped
    stopping ( StoppedString        _ _   ) = Stopped
    stopping ( StoppedStringTrem    _ _ _ ) = Stopped
    stopping ( StoppedStringGliss   _ _ _ ) = Stopped

instance Stopped a => Stopped (RightHand t r p a) where
    stopping ( Pizz   _ x )       =  stopping x
    stopping ( Single _ x )       =  stopping x
    stopping ( Phrase _ (x:xs) )  =  stopping (snd x)
    stopping ( Jete   _ (x:xs) )  =  stopping x

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
cueIntonation (Cue p d n t) = intonation d t

raisedIntonation :: Cent
raisedIntonation = 23 Cent

\end{code}

\pagebreak




Cues
----------

A *cue* is an action taken by a performer on time.

\begin{code}
type Technique =
    RightHand
        Dur
        Articulation
        Phrasing
        (LeftHand
            Pitch Str)

data Cue
    = Cue
    {
        cuePart      :: Part,
        cueDoubling  :: Doubling,
        cueDynamics  :: Dynamics,  -- time is 0 to 1 for the duration of the cue
        cueTechnique :: Technique
    }
    deriving ( Eq, Show )

mapCuePart      :: (Part -> Part)           -> Cue -> Cue
mapCueDoubling  :: (Doubling -> Doubling)   -> Cue -> Cue
mapCueDynamics  :: (Dynamics -> Dynamics)   -> Cue -> Cue
mapCueTechnique :: (Technique -> Technique) -> Cue -> Cue

mapCuePart      f (Cue p d n t) = Cue (f p) d n t
mapCueDoubling  f (Cue p d n t) = Cue p (f d) n t
mapCueDynamics  f (Cue p d n t) = Cue p d (f n) t
mapCueTechnique f (Cue p d n t) = Cue p d n (f t)

setCuePart p = mapCuePart (const p)
setCueDoubling d = mapCueDoubling (const d)
setCueDynamics n = mapCueDynamics (const n)
setCueTechnique t = mapCueTechnique (const t)

\end{code}

\pagebreak






Rendering
==========

We are going to compose the piece as a score of cues. In order to hear the piece
and make musical decisions, we need to define a rendering function that renders a 
cue to a score of Midi notes, which is the object of this chapter.

The `MidiNote` type is imported from `Music.Render.Midi`, but we define some
extra type synonyms to make the rendering functions somewhat more readable:

\begin{code}
type MidiChannel    = Int
type MidiInstrument = Maybe Int
type MidiPitch      = Int
type MidiBend       = Semitones
type MidiDynamic    = Int


\end{code}
Channel
----------

A caveat is that the Midi representation does not handle simultaneous tunings well.
We must therefore separete the music into different Midi channels based on part, section
and intontation.

\begin{code}
midiChannel :: Cue -> MidiChannel
midiChannel (Cue part doubling dynamics technique) =
    midiChannel' part section intonation'
    where
          section     = partSection part
          intonation' = intonation doubling technique

midiChannel'  ( Violin _)   High   Tuning      =  0
midiChannel'  ( Viola  _ )  High   Tuning      =  1
midiChannel'  ( Cello  _ )  High   Tuning      =  2
midiChannel'  ( Violin _)   Low    Tuning      =  3
midiChannel'  ( Viola  _ )  Low    Tuning      =  4
midiChannel'  ( Cello  _ )  Low    Tuning      =  5
midiChannel'  ( Violin _)   _      Common      =  6
midiChannel'  ( Viola  _ )  _      Common      =  7
midiChannel'  ( Cello  _ )  _      Common      =  8

midiChannel'  DoubleBass    _      _           =  10

midiChannel'  ( Violin _)   _      Raised      =  11
midiChannel'  ( Viola  _ )  _      Raised      =  11
midiChannel'  ( Cello  _ )  _      Raised      =  13
midiChannel'  ( Violin _)   High   Individual  =  0
midiChannel'  ( Viola  _ )  High   Individual  =  1
midiChannel'  ( Cello  _ )  High   Individual  =  2
midiChannel'  ( Violin _)   Low    Individual  =  3
midiChannel'  ( Viola  _ )  Low    Individual  =  4
midiChannel'  ( Cello  _ )  Low    Individual  =  5


\end{code}
Instrument
----------

Instrument rendering is simple: if the technique is *pizzicato*, use the pizzicato
strings program, otherwise use the program representing the current instrument.

(The standard programs give us solo sounds. We could mix in the *string ensemble*
program based on the doubling  attribute. I am not sure this is a good idea though.)

\begin{code}
midiInstrument :: Cue -> MidiInstrument
midiInstrument (Cue part doubling dynamics technique) =
    case technique of
        (Pizz _ _) -> Just 45
        _          -> midiInstrument' part

midiInstrument' ( Violin _ )  =  Just 40
midiInstrument' ( Viola _ )   =  Just 41
midiInstrument' ( Cello _ )   =  Just 42
midiInstrument' DoubleBass    =  Just 43


\end{code}
Pitch and bending
----------

Table of open string pitches.

\begin{code}
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


\end{code}

Determine amount of pitch bend (in semitones) from the part, doubling and technique.
Note that the `cents` function converts a frequency to cents, so by subtracting the
reference pitch from the intonation, we get the amount of bending in cents. Then
divide this by 100 to get the amount in semitones.

\begin{code}
midiBend :: Cue -> MidiBend
midiBend (Cue part doubling dynamics technique) =
    midiBend' (intonation', cents')
    where
        intonation'   = intonation doubling technique
        tuning'       = partTuning part
        cents'        = cents tuning' - cents 440

midiBend' ( Raised, c )     = getCent (c + raisedIntonation) / 100
midiBend' ( Tuning, c )     = getCent c / 100
midiBend' ( Common, c )     = 0
midiBend' ( Individual, c ) = 0

\end{code}

Left hand
----------
The `renderLeftHand` function returns a score of duration one, possibly containing tremolos.

\begin{code}

renderLeftHand :: Part -> LeftHand Pitch Str -> TremoloScore Dur MidiNote
renderLeftHand part (OpenString           s)      =  renderLeftHandSingle (openStringPitch part s)
renderLeftHand part (NaturalHarmonic      x s)    =  renderLeftHandSingle x
renderLeftHand part (NaturalHarmonicTrem  x y s)  =  renderLeftHandTrem x y
renderLeftHand part (NaturalHarmonicGliss x y s)  =  renderLeftHandGliss
renderLeftHand part (QuarterStoppedString s)      =  renderLeftHandSingle (openStringPitch part s)
renderLeftHand part (StoppedString        x s)    =  renderLeftHandSingle x
renderLeftHand part (StoppedStringTrem    x y s)  =  renderLeftHandTrem x y
renderLeftHand part (StoppedStringGliss   x y s)  =  renderLeftHandGliss

\end{code}

TODO gliss.

\begin{code}
renderLeftHandSingle x   = note . Left  $ MidiNote 0 Nothing x 0 60
renderLeftHandTrem   x y = note . Right $ tremoloBetween tremoloInterval (MidiNote 0 Nothing x 0 60) (MidiNote 0 Nothing y 0 60)
renderLeftHandGliss      = error "Gliss not implemented"

tremoloInterval = 0.08

\end{code}

This section needs some cleanup.

\begin{code}
setMidiChannel :: MidiChannel -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiChannel c = fmapE f g
    where f = (\(MidiNote _ i p b n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote _ i p b n) -> MidiNote c i p b n)

setMidiInstrument :: MidiInstrument -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiInstrument i = fmapE f g
    where f = (\(MidiNote c _ p b n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote c _ p b n) -> MidiNote c i p b n)

setMidiBend :: MidiBend -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiBend b = fmapE f g
    where f = (\(MidiNote c i p _ n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote c i p _ n) -> MidiNote c i p b n)

setMidiDynamic :: Dynamics -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiDynamic (Dynamics n) = tmapE f g
    where f = (\t (MidiNote c i p b _) -> MidiNote c i p b (round $ n t * 63 + 63))
          g = (\t x -> tmap (\t (MidiNote c i p b _) -> MidiNote c i p b (round $ n t * 63 + 63)) x)

\end{code}
Right hand and cues
------
\begin{code}

renderCue :: Cue -> TremoloScore Dur MidiNote
renderCue cue@(Cue part doubling dynamics technique) =
    postHoc $ case technique of
        Pizz   art leftHand  -> renderLeftHand part leftHand
        Single art leftHand  -> renderLeftHand part leftHand
        Phrase art leftHand  -> renderLeftHands leftHand
        Jete   art leftHand  -> renderLeftHands (zip bounceDur leftHand)
    where
        renderLeftHands   =  stretchTo 1 . concatSeq . map leftHands
        leftHands (d, x)  =  stretch d $ renderLeftHand part x

        channel    =  midiChannel cue
        instr      =  midiInstrument cue
        bend       =  midiBend cue

        postHoc    =  setMidiChannel channel
                   .  setMidiInstrument instr
                   .  setMidiBend bend
                   .  setMidiDynamic dynamics

\end{code}

Basic rendering of jeté bow strokes.

\begin{code}
bounceDur :: [Dur]
bounceDur  =  [ (2 ** (-0.9 * x)) / 6 | x <- [0,0.1..1.2] ]

bounceVel :: [Double]
bounceVel  =  [ abs (1 - x) | x <- [0,0.08..]]


\end{code}

This instance makes it possible to use the `play` function on scores of cues:

\begin{code}
instance Render (Score Dur Cue) Midi where
    render = render
           . restAfter 5
           . renderTremoloEvents
           . (>>= renderCue)
\end{code}

\pagebreak





High-level constructors
==========

Allthough the *cues* defined in the previous chapters is a flexible representation for an
orchestral piece, they are also very verbose and cubersome to construct. This is easily
solved by adding some higher-level constructors.

The constructors all create *standard cues* with the following definition:

\begin{code}
standardCue = note . Cue (Violin 1) Tutti mf
\end{code}

The `cueSet...` family of functors along with `fmap` can be used to change these default
values.

Open Strings
----------
\begin{code}
openString :: Str -> Score Dur Cue
openString x = standardCue 
    $ Single Straight 
    $ OpenString x

openStringPizz :: Str -> Score Dur Cue
openStringPizz x = standardCue
     $ Pizz Straight 
     $ OpenString x

openStringJete :: [Str] -> Score Dur Cue
openStringJete xs = standardCue
     $ Jete Phrasing 
     $ map OpenString xs

openStrings :: [(Dur, Str)] -> Score Dur Cue
openStrings xs = standardCue
     $ Phrase Phrasing 
     $ map (\(d,x) -> (d, OpenString x)) xs


\end{code}
Natural harmonics
----------
\begin{code}
naturalHarmonic :: Pitch -> Score Dur Cue
naturalHarmonic x = standardCue
     $ Single Straight 
     $ NaturalHarmonic x I

naturalHarmonicPizz :: Pitch -> Score Dur Cue
naturalHarmonicPizz x = standardCue
     $ Pizz Straight 
     $ NaturalHarmonic x I

naturalHarmonicJete :: [Pitch] -> Score Dur Cue
naturalHarmonicJete xs = standardCue
     $ Jete Phrasing 
     $ map (\x -> NaturalHarmonic x I) xs

naturalHarmonics :: [(Dur, Pitch)] -> Score Dur Cue
naturalHarmonics pitch = standardCue
     $ Phrase Phrasing 
     $ map (\(d,x) -> (d, NaturalHarmonic x I)) pitch


\end{code}
Quarter stopped strings
----------
\begin{code}
quarterStoppedString :: Str -> Score Dur Cue
quarterStoppedString x = standardCue
     $ Single Straight 
     $ QuarterStoppedString x

quarterStoppedStrings :: [(Dur, Str)] -> Score Dur Cue
quarterStoppedStrings xs = standardCue
     $ Phrase Phrasing 
     $ map (\(d, x) -> (d, QuarterStoppedString x)) xs


\end{code}
Stopped strings
----------
\begin{code}
stoppedString :: Pitch -> Score Dur Cue
stoppedString x = standardCue
     $ Single Straight 
     $ StoppedString x I

stoppedStringPizz :: Pitch -> Score Dur Cue
stoppedStringPizz x = standardCue
     $ Pizz Straight 
     $ StoppedString x I

stoppedStringJete :: [Pitch] -> Score Dur Cue
stoppedStringJete xs = standardCue
     $ Jete Phrasing 
     $ map (\x -> StoppedString x I) xs

stoppedStrings :: [(Dur, Pitch)] -> Score Dur Cue
stoppedStrings xs = standardCue
     $ Phrase Phrasing 
     $ map (\(d, x) -> (d, StoppedString x I)) xs

                                 
\end{code}
Tremolo
----------
\begin{code}
stoppedStringTrem :: Pitch -> Pitch -> Score Dur Cue
stoppedStringTrem x y = standardCue
     $ Single Straight 
     $ StoppedStringTrem x y I

naturalHarmonicTrem :: Pitch -> Pitch -> Score Dur Cue
naturalHarmonicTrem x y = standardCue
     $ Single Straight 
     $ NaturalHarmonicTrem x y I

\end{code}

\pagebreak





Final composition
==========

In this chapter we will assemble the final piece.

Pitch material
----------
\begin{code}

type Scale = [Pitch]
minScale = scaleFrom [0,2,1,2,2,1,2,2]
majMinScale = (retrograde . invert) minScale ++ tail minScale



scaleFrom :: [Pitch] -> Scale
scaleFrom = List.reverse . foldl (\ys x -> x + (if (null ys) then 0 else head ys) : ys) []

retrograde :: [Pitch] -> [Pitch]
retrograde = List.reverse

invert :: [Pitch] -> [Pitch]
invert = map negate










\end{code}

\pagebreak






Form
----------

\begin{code}
test :: Score Dur Cue
test =
         (stretch 7  . fmap (setCueDynamics f) . stoppedStrings $ zip ([1,1.1..]) (map (+67) majMinScale))
     ||| (stretch 8  . fmap (setCueDynamics f) . stoppedStrings $ zip ([1,1.1..]) (map (+67) majMinScale))
     ||| (stretch 9  . fmap (setCueDynamics f) . stoppedStrings $ zip ([1,1.1..]) (map (+67) majMinScale))
     ||| (stretch 10 . fmap (setCueDynamics p) $ openString III)
     ||| (stretch 10 . fmap (setCueDynamics p) $ openString II)

--test =   (delay 0   . stretch 10) (tremStopped (Cello 1) 55 57)
--     ||| (delay 0.2 . stretch 3) (tremStopped (Cello 1) 48 50)


-- test = oj' ||| allOpenStrings
--     where oj = openStringJete (Viola 1) $ cycle (enumFromTo I IV)
--           oj' = before 10 (loop oj)


-- allOpenStrings :: Score Dur Cue
-- allOpenStrings =
--     stretch (1/3)
--         $   concatSeq [ openString DoubleBass   str | str <- enumFrom I ]
--         >>> concatSeq [ openString (instr part) str | instr <- [Cello, Viola, Violin]
--                                                     , part  <- [2,1]
--                                                     , str   <- enumFrom I ]



main = writeMidi "Passager.mid" (render test)

\end{code}


