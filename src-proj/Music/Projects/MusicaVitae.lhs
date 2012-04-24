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
    NoMonomorphismRestriction,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    DeriveFunctor #-}

module Music.Projects.MusicaVitae
where

import Prelude hiding ( reverse )

import Control.Concurrent
import Data.Maybe
import Data.Convert ( convert )
import Data.Trivial
import Data.Index
import Data.Char
import Data.Ord ( comparing )
import System.Random
import System.IO.Unsafe

import Music
import Music.Inspect
import Music.Time.Event
import Music.Time.EventList
import Music.Time.Tremolo
import Music.Time.Functors
import Music.Render
import Music.Render.Midi
import Music.Render.Graphics
import Music.Util
import Music.Util.System ( execute )
import qualified Music.Util.List as List
import qualified Music.Util.Either as Either

import Music.Notable.Core
import Music.Notable.Core.Diagrams hiding (Time, stretch, stretchTo, duration, during, after)
import Music.Notable.Engraving hiding (rest, Articulation, fff, ff, f, mf, mp, p, pp, ppp)
import qualified Music.Notable.Engraving as Notable

import Music.Projects.MusicaVitae.Random


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

instance Enum Part where
    fromEnum x = fromJust $ List.findIndex (== x) ensemble
    toEnum x = ensemble !! x

instance Bounded Part where
    minBound = Violin 1
    maxBound = DoubleBass

data Section
    = High | Low | Middle
    deriving ( Eq, Show, Enum, Bounded )

type Tuning = Frequency
\end{code}


We now define the relation between these types as follows:

\begin{code}
partSection   :: Part -> Section
sectionTuning :: Section -> Tuning
partTuning    :: Part -> Tuning

partSection ( Violin 1 ) = High
partSection ( Violin 2 ) = Low
partSection ( Violin 3 ) = High
partSection ( Violin 4 ) = Low
partSection ( Viola 1 )  = High
partSection ( Viola 2 )  = Low
partSection ( Cello 1 )  = High
partSection ( Cello 2 )  = Low
partSection DoubleBass = Middle

sectionTuning Low    = 440 - 0
sectionTuning Middle = 440
sectionTuning High   = 440 + 0

partTuning = sectionTuning . partSection
\end{code}


Then add some utility definitions to quickly access the various parts:

\begin{code}
ensemble     :: [Part]
sectionParts :: Section -> [Part]

isViolin, isViola, isCello, isDoubleBass :: Part -> Bool

highParts, lowParts
    :: [Part]
highViolins, highViolas, highCellos
    :: [Part]
lowViolins, lowViolas, lowCellos
    :: [Part]
doubleBass :: Part

ensemble
    = [ Violin 1, Violin 2, Violin 3, Violin 4
      , Viola 1, Viola 2, Cello 1, Cello 2, DoubleBass ]

sectionParts s  =  filter (\x -> partSection x == s) ensemble

highParts  =  sectionParts High
lowParts   =  sectionParts Low

isViolin     ( Violin _ )    =  True
isViolin     _               =  False
isViola      ( Viola _ )     =  True
isViola      _               =  False
isCello      ( Cello _ )     =  True
isCello      _               =  False
isDoubleBass ( DoubleBass )  =  True
isDoubleBass _               =  True

violins  =  filter isViolin ensemble
violas   =  filter isViola  ensemble
cellos   =  filter isCello  ensemble
highViolins  =  filter isViolin (sectionParts High)
highViolas   =  filter isViola  (sectionParts High)
highCellos   =  filter isCello  (sectionParts High)
lowViolins   =  filter isViolin (sectionParts Low)
lowViolas    =  filter isViola  (sectionParts Low)
lowCellos    =  filter isCello  (sectionParts Low)
doubleBass   =  DoubleBass
\end{code}

Part names, for output:

\begin{code}
partName :: Part -> String

partName ( Violin 1 ) = "Violin I.1"
partName ( Violin 2 ) = "Violin I.2"
partName ( Violin 3 ) = "Violin II.1"
partName ( Violin 4 ) = "Violin II.2"
partName ( Viola 1 )  = "Viola I"
partName ( Viola 2 )  = "Viola II"
partName ( Cello 1 )  = "Violoncell I"
partName ( Cello 2 )  = "Violoncell II"
partName DoubleBass   = "Kontrabas"

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


The `PartFunctor` class defined useful operations for mapping over part and doubling.

\begin{code}
class PartFunctor f where
    setPart     :: Part -> f -> f
    mapPart     :: (Part -> Part) -> f -> f

    setDoubling :: Doubling -> f -> f
    mapDoubling :: (Doubling -> Doubling) -> f -> f

    setPart     x = mapPart (const x)
    setDoubling x = mapDoubling (const x)
\end{code}





Time and pitch
----------

We will use the temporal operations form `Music.Time` for composition on both event level
and structural level. We use floating point values to represent durations.

\begin{code}
type Dur = Double
\end{code}


For simplicity, we will use MIDI numbers for written pitch. Sounding pitch will of course
be rendered depending on tuning and playing technique of the given part.

String number will be represented separately using a different type (named `Str` so as not
to collide with `String`).

\begin{code}
type Pitch = Int

pitch :: Int -> Pitch
pitch = id

data Str = I | II | III | IV
    deriving ( Eq, Show, Ord, Enum, Bounded )
\end{code}


A scale is conceptually a function from steps to pitches. This relation is captured by
the `step` function, which maps steps to pitches. For example, `major 'step' 3` means
the third step in the major scale.

The simplest way to generate a scale is to list its relative steps, i.e. `0,2,2,1,2` for
the first five pitches in the major scale. This is captured by the function `scaleFromSteps`.

\begin{code}
newtype Scale a = Scale { getScale :: [a] }
    deriving ( Eq, Show, Functor )

step :: Scale Pitch -> Pitch -> Pitch
step (Scale xs) p = xs !! (p `mod` length xs)

fromStep :: Scale Pitch -> Pitch -> Pitch
fromStep (Scale xs) p = fromMaybe (length xs - 1) $ List.findIndex (>= p) xs

scaleFromSteps :: [Pitch] -> Scale Pitch
scaleFromSteps = Scale . accum
    where
        accum = snd . List.mapAccumL add 0
        add a x = (a + x, a + x)

numberOfSteps :: Scale a -> Int
numberOfSteps = length . getScale

major :: Scale Pitch
major = scaleFromSteps [0,2,2,1,2,2,2,1]

naturalMinor :: Scale Pitch
naturalMinor = scaleFromSteps [0,2,1,2,2,1,2,2]

harmonicMinor :: Scale Pitch
harmonicMinor = scaleFromSteps [0,2,1,2,2,1,3,1]

retrograde :: Scale Pitch -> Scale Pitch
retrograde = Scale . List.reverse . getScale
\end{code}


The `PitchFunctor` class defines a useful operation for mapping over pitch. This
generalizes to scales and scores containing pitched elements.

\begin{code}
class PitchFunctor f where
    setPitch :: Pitch -> f -> f
    mapPitch :: (Pitch -> Pitch) -> f -> f
    setPitch x = mapPitch (const x)

instance PitchFunctor (Pitch) where
    mapPitch f x = f x

instance PitchFunctor (Scale Pitch) where
    mapPitch f = fmap f

invert :: PitchFunctor f => f -> f
invert = mapPitch negate

invertAround :: PitchFunctor f => Pitch -> f -> f
invertAround p = mapPitch (+ p) . invert . mapPitch (subtract p)

\end{code}

Even though all offsets and durations in the piece will be manged by the `Music.Time` functionality,
we need to keep track of tempo information separately for the purposes of notation. The `TempoFunctor`
class will take care of this.

\begin{code}
type Tempo = Double

class TempoFunctor f where
    setTempo     :: Tempo -> f -> f
    mapTempo     :: (Tempo -> Tempo) -> f -> f
    setTempo x = mapTempo (const x)
\end{code}


Finally, some instances to make it possible to use the `play` function on pitches and scales.
These arbitrarily assume a reference pitch of 60 (middle C).

\begin{code}
instance Render Pitch Midi where
    render pitch = render $ n pitch
        where n = note :: Pitch -> Score Dur Pitch

instance Render (Scale Pitch) Midi where
    render scale = render . compress 2 . l . fmap (step scale) $ [0..n-1]
        where l = line :: [Pitch] -> Score Dur Pitch
              n = numberOfSteps scale

instance Render (Score Dur Pitch) Midi where
    render = render . fmap (\p -> MidiNote 0 Nothing (p + 60) 0 60)
\end{code}





Dynamics
----------

We use a linear representation for dynamic levels. Level 0 corresponds to some medium level
dynamic, level 1 to extremely loud and level -1 to extremely soft. A dynamic is a function
from time to level, generalizing crescendo, diminuendo and so on.

\begin{code}
type Level = Double

newtype Dynamics = Dynamics { getDynamics :: Dur -> Level }
    deriving ( Eq )

instance Show Dynamics where
    show x = ""

levelAt :: Dynamics -> Dur -> Level
levelAt (Dynamics n) t = n t

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

class LevelFunctor f where
    setLevel :: Level -> f -> f
    mapLevel :: (Level -> Level) -> f -> f
    setLevel x = mapLevel (const x)

    setDynamics :: Dynamics -> f -> f
    mapDynamics :: (Dur -> Level -> Level) -> f -> f
    setDynamics n = mapDynamics (\t _ -> n `levelAt` t)

instance LevelFunctor Dynamics where
    mapLevel f (Dynamics n) = Dynamics (f . n)
    mapDynamics f (Dynamics n) = Dynamics (\t -> f t $ n t)
\end{code}







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

leftHand :: RightHand t c r a -> [a]
leftHand (Pizz   c x)   =  [x]
leftHand (Single c x)   =  [x]
leftHand (Phrase c xs)  =  map snd xs
leftHand (Jete   c xs)  =  xs

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

class HasDur a where
    getDur :: a -> Dur

-- class HasPitch a where
    -- getPitch :: Part -> a -> Pitch

instance Stopped (LeftHand p s) where
    stopping ( OpenString           s     ) = Open
    stopping ( NaturalHarmonic      x s   ) = Open
    stopping ( NaturalHarmonicTrem  x y s ) = Open
    stopping ( NaturalHarmonicGliss x y s ) = Open
    stopping ( QuarterStoppedString s     ) = QuarterStopped
    stopping ( StoppedString        x s   ) = Stopped
    stopping ( StoppedStringTrem    x y s ) = Stopped
    stopping ( StoppedStringGliss   x y s ) = Stopped

instance Stopped a => Stopped (RightHand t r p a) where
    stopping ( Pizz   c x )       =  stopping x
    stopping ( Single c x )       =  stopping x
    stopping ( Phrase r (x:xs) )  =  stopping (snd x)
    stopping ( Jete   r (x:xs) )  =  stopping x

instance Time t => HasDur (RightHand t r p a) where
    getDur ( Pizz   c x )   =  1
    getDur ( Single c x )   =  1
    getDur ( Phrase r xs )  =  timeToDouble . sum . fst . unzip $ xs
    getDur ( Jete   r xs )  =  1

-- instance HasPitch (LeftHand Pitch Str) where
--     getPitch r ( OpenString           s     ) = openStringPitch r s
--     getPitch r ( NaturalHarmonic      x s   ) = 0
--     getPitch r ( NaturalHarmonicTrem  x y s ) = 0
--     getPitch r ( NaturalHarmonicGliss x y s ) = 0
--     getPitch r ( QuarterStoppedString s     ) = 0
--     getPitch r ( StoppedString        x s   ) = 0
--     getPitch r ( StoppedStringTrem    x y s ) = 0
--     getPitch r ( StoppedStringGliss   x y s ) = 0
--
-- instance HasPitch a => HasPitch (RightHand t r Pitch a) where
--     getPitch r ( Pizz   c x )       =  getPitch r x
--     getPitch r ( Single c x )       =  getPitch r x
--     getPitch r ( Phrase a (x:xs) )  =  getPitch r (snd x)
--     getPitch r ( Jete   a (x:xs) )  =  getPitch r x


instance PitchFunctor (LeftHand Pitch s) where
    mapPitch f ( StoppedString        x s   ) = StoppedString      (f x) s
    mapPitch f ( StoppedStringTrem    x y s ) = StoppedStringTrem  (f x) (f y) s
    mapPitch f ( StoppedStringGliss   x y s ) = StoppedStringGliss (f x) (f y) s
    mapPitch f x                              = x

instance PitchFunctor a => PitchFunctor (RightHand t c r a) where
    mapPitch f ( Pizz   c x )   =  Pizz   c (mapPitch f x)
    mapPitch f ( Single c x )   =  Single c (mapPitch f x)
    mapPitch f ( Phrase r xs )  =  Phrase r (fmap (\(d,p) -> (d, mapPitch f p)) xs)
    mapPitch f ( Jete   r xs )  =  Jete   r (fmap (mapPitch f) xs)
\end{code}








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
cueIntonation (Cue p d n e m t) = intonation d t

raisedIntonation :: Cent
raisedIntonation = 23 Cent
\end{code}







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

isPhrase :: Technique -> Bool
isPhrase (Phrase _ _) = True
isPhrase _            = False

data Mark = RehearsalMark
    deriving ( Eq, Show )

data Cue
    = Cue
    {
        cuePart      :: Part,
        cueDoubling  :: Doubling,
        cueDynamics  :: Dynamics,  -- time is 0 to 1 for the duration of the cue
        cueTempo     :: Tempo,
        cueMarks     :: [Mark],
        cueTechnique :: Technique
    }
    deriving ( Eq, Show )

instance PartFunctor Cue where
    mapPart     f (Cue p d n e m t) = Cue (f p) d n e m t
    mapDoubling f (Cue p d n e m t) = Cue p (f d) n e m t

instance LevelFunctor Cue where
    mapLevel f    (Cue p d n e m t) = Cue p d (mapLevel f n) e m t
    mapDynamics f (Cue p d n e m t) = Cue p d (mapDynamics f n) e m t

instance TempoFunctor Cue where
    mapTempo     f (Cue p d n e m t) = Cue p d n (f e) m t

instance PitchFunctor Cue where
    mapPitch f (Cue p d n e m t) = Cue p d n e m (mapPitch f t)

instance (Time t, PartFunctor a) => PartFunctor (Score t a) where
    mapPart f = fmap (mapPart f)
    mapDoubling f = fmap (mapDoubling f)

instance (Time t, LevelFunctor a) => LevelFunctor (Score t a) where
    mapLevel    f = fmap (mapLevel f)
    mapDynamics f = fmap (mapDynamics f)

instance (Time t, PitchFunctor a) => PitchFunctor (Score t a) where
    mapPitch f = fmap (mapPitch f)

\end{code}


Cue generation
==========

Allthough the *cues* defined in the previous chapter is a flexible representation for an
orchestral piece, they are somewhat cubersome to construct. This is easily solved by
adding some higher-level constructors.

The constructors all create *standard cues* with the following definitions:

\begin{code}
standardCue           =  note . Cue (Violin 1) Tutti mf 0 []
standardArticulation  =  Straight
standardPhrasing      =  Phrasing
\end{code}

These can be overriden using the methods of the type classes `Temporal`, `Timed`, `Delayed`,
`PartFunctor`, `PitchFunctor` and `LevelFunctor` respectively.



Open Strings
----------
\begin{code}
openString :: Str -> Score Dur Cue
openString x = standardCue
    $ Single standardArticulation
    $ OpenString x

openStringPizz :: Str -> Score Dur Cue
openStringPizz x = standardCue
     $ Pizz standardArticulation
     $ OpenString x

openStringJete :: [Str] -> Score Dur Cue
openStringJete xs = standardCue
     $ Jete standardPhrasing
     $ map OpenString xs

openStrings :: [(Dur, Str)] -> Score Dur Cue
openStrings xs = standardCue
     $ Phrase standardPhrasing
     $ map (\(d,x) -> (d, OpenString x)) xs
\end{code}




Natural harmonics
----------
\begin{code}
naturalHarmonic :: Str -> Pitch -> Score Dur Cue
naturalHarmonic s x = standardCue
     $ Single standardArticulation
     $ NaturalHarmonic x s

naturalHarmonicPizz :: Str -> Pitch -> Score Dur Cue
naturalHarmonicPizz s x = standardCue
     $ Pizz standardArticulation
     $ NaturalHarmonic x s

naturalHarmonicJete :: Str -> [Pitch] -> Score Dur Cue
naturalHarmonicJete s xs = standardCue
     $ Jete standardPhrasing
     $ map (\x -> NaturalHarmonic x s) xs

naturalHarmonics :: Str -> [(Dur, Pitch)] -> Score Dur Cue
naturalHarmonics s xs = standardCue
     $ Phrase standardPhrasing
     $ map (\(d,x) -> (d, NaturalHarmonic x s)) xs
\end{code}



Quarter stopped strings
----------
\begin{code}
quarterStoppedString :: Str -> Score Dur Cue
quarterStoppedString x = standardCue
     $ Single standardArticulation
     $ QuarterStoppedString x

quarterStoppedStrings :: [(Dur, Str)] -> Score Dur Cue
quarterStoppedStrings xs = standardCue
     $ Phrase standardPhrasing
     $ map (\(d,x) -> (d, QuarterStoppedString x)) xs
\end{code}



Stopped strings
----------
\begin{code}
stoppedString :: Pitch -> Score Dur Cue
stoppedString x = standardCue
     $ Single standardArticulation
     $ StoppedString x I

stoppedStringPizz :: Pitch -> Score Dur Cue
stoppedStringPizz x = standardCue
     $ Pizz standardArticulation
     $ StoppedString x I

stoppedStringJete :: [Pitch] -> Score Dur Cue
stoppedStringJete xs = standardCue
     $ Jete standardPhrasing
     $ map (\x -> StoppedString x I) xs

stoppedStrings :: [(Dur, Pitch)] -> Score Dur Cue
stoppedStrings xs = standardCue
     $ Phrase standardPhrasing
     $ map (\(d,x) -> (d, StoppedString x I)) xs
\end{code}




Tremolo
----------
\begin{code}
stoppedStringTrem :: Pitch -> Pitch -> Score Dur Cue
stoppedStringTrem x y = standardCue
     $ Single standardArticulation
     $ StoppedStringTrem x y I

naturalHarmonicTrem :: Str -> Pitch -> Pitch -> Score Dur Cue
naturalHarmonicTrem s x y = standardCue
     $ Single standardArticulation
     $ NaturalHarmonicTrem x y s
\end{code}


\pagebreak






Playback
==========

We are going to compose the piece as a score of cues. In order to hear the piece
and make musical decisions, we need to define a rendering function that renders a
cue to a score of MIDI notes, which is the object of this chapter.

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

A caveat is that the MIDI representation does not handle simultaneous tunings well.
We must therefore separete the music into different MIDI channels based on part, section
and intontation.

\begin{code}
midiChannel :: Cue -> MidiChannel
midiChannel (Cue part doubling dynamics tempo marks technique) =
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
midiInstrument (Cue part doubling dynamics tempo marks technique) =
    case technique of
        (Pizz _ _) -> Just 45
        _          -> midiInstrument' part

midiInstrument' _             =  Just 49
-- midiInstrument' ( Violin _ )  =  Just 40
-- midiInstrument' ( Viola _ )   =  Just 41
-- midiInstrument' ( Cello _ )   =  Just 42
-- midiInstrument' DoubleBass    =  Just 43

\end{code}



Pitch and bend
----------

Table of open string pitches.

\begin{code}
openStringPitch :: Part -> Str -> MidiPitch
openStringPitch ( Violin _ ) I    =  55
openStringPitch ( Violin _ ) II   =  62
openStringPitch ( Violin _ ) III  =  69
openStringPitch ( Violin _ ) IV   =  76
openStringPitch ( Viola _ )  I    =  48
openStringPitch ( Viola _ )  II   =  55
openStringPitch ( Viola _ )  III  =  62
openStringPitch ( Viola _ )  IV   =  69
openStringPitch ( Cello _ )  I    =  36
openStringPitch ( Cello _ )  II   =  43
openStringPitch ( Cello _ )  III  =  50
openStringPitch ( Cello _ )  IV   =  57
openStringPitch DoubleBass   I    =  28
openStringPitch DoubleBass   II   =  33
openStringPitch DoubleBass   III  =  38
openStringPitch DoubleBass   IV   =  43

naturalHarmonicPitch :: Part -> Str -> Int -> MidiPitch
naturalHarmonicPitch part str tone =
    fundamental + overtone
    where
        fundamental = openStringPitch part str
        overtone = scaleFromSteps [0,12,7,5,4,3,3,2,2,2] `step` tone

naturalHarmonicWrittenPitch :: Part -> Str -> Pitch -> Pitch
naturalHarmonicWrittenPitch part str tone =
    fundamental + position
    where
        fundamental = openStringPitch part str
        position = [0,12,7,5,4,3] !! tone

\end{code}




We determine amount of pitch bend from the part, doubling and technique.
Note that the `cents` function converts a frequency to cents, so by subtracting the
reference pitch from the intonation, we get the amount of bending in cents. Then
divide this by 100 to get the amount in semitones.

For harmonics, we add a compensation for the difference between just and
twelve-tone equal temperament. Unfortunately this does not work for harmonic tremolos.

\begin{code}
midiBend :: Cue -> MidiBend
midiBend (Cue part doubling dynamics tempo marks technique) =
    midiBend' (intonation', cents') + just
    where
        intonation'  =  intonation doubling technique
        tuning'      =  partTuning part
        cents'       =  cents tuning' - cents 440
        just         =  midiBendJust (head . leftHand $ technique)

midiBend' ( Raised, c )      =  getCent (c + raisedIntonation) / 100
midiBend' ( Tuning, c )      =  getCent c / 100
midiBend' ( Common, c )      =  0
midiBend' ( Individual, c )  =  0

midiBendJust :: LeftHand Pitch Str -> MidiBend
midiBendJust ( NaturalHarmonic x s )  =  midiBendJust' x
midiBendJust _                        = 0

midiBendJust' 0  =  0
midiBendJust' 1  =  0
midiBendJust' 2  =  0.0196
midiBendJust' 3  =  0
midiBendJust' 4  =  -0.1369
midiBendJust' 5  =  0.0196
midiBendJust' 6  =  -0.3117
midiBendJust' 7  =  0
midiBendJust' 8  =  0.0391

\end{code}




Velocity
--------

\begin{code} 
partDynamic :: Part -> Dynamics -> Dynamics    
partDynamic (Violin _)      =  mapLevel (* 1)
partDynamic (Viola _)       =  mapLevel (* 0.9)
partDynamic (Cello _)       =  mapLevel (* 0.8)
partDynamic DoubleBass      =  mapLevel (* 0.7)

renderDynamic' :: Part -> Dynamics -> Dur -> MidiDynamic
renderDynamic' r = renderDynamic . partDynamic r
    
renderDynamic :: Dynamics -> Dur -> MidiDynamic
renderDynamic n t = round . d $ n `levelAt` t
    where
        -- d x = x * 63 + 63
        d x = ((x + 1) / 2 ** 1.2) * 128

\end{code}

Left hand
----------
The `renderLeftHand` function returns a score of duration one, possibly containing tremolos.
This property is formalized by the use of a `TremoloScore`, i.e. a score containing either
notes or tremolos.

TODO gliss

\begin{code}
renderLeftHand :: Part -> LeftHand Pitch Str -> TremoloScore Dur MidiNote
renderLeftHand part ( OpenString           s )      =  renderLeftHandSingle (openStringPitch part s)
renderLeftHand part ( NaturalHarmonic      x s )    =  renderLeftHandSingle (naturalHarmonicPitch part s x)
renderLeftHand part ( NaturalHarmonicTrem  x y s )  =  renderLeftHandTrem (naturalHarmonicPitch part s x) (naturalHarmonicPitch part s y)
renderLeftHand part ( NaturalHarmonicGliss x y s )  =  renderLeftHandGliss
renderLeftHand part ( QuarterStoppedString s )      =  renderLeftHandSingle (openStringPitch part s)
renderLeftHand part ( StoppedString        x s )    =  renderLeftHandSingle x
renderLeftHand part ( StoppedStringTrem    x y s )  =  renderLeftHandTrem x y
renderLeftHand part ( StoppedStringGliss   x y s )  =  renderLeftHandGliss

renderLeftHandSingle x    =  note . Left  $ renderMidiNote x
renderLeftHandTrem   x y  =  note . Right $ tremoloBetween kTremoloInterval (renderMidiNote x) (renderMidiNote y)
renderLeftHandGliss       =  error "Gliss not implemented"

renderMidiNote x = MidiNote 0 Nothing x 0 60

kTremoloInterval = 0.08
\end{code}




Right hand
----------
\begin{code}
renderRightHand :: Part -> Technique -> TremoloScore Dur MidiNote
renderRightHand part ( Pizz   articulation leftHand )  = renderLeftHand part leftHand
renderRightHand part ( Single articulation leftHand )  = renderLeftHand part leftHand
renderRightHand part ( Phrase phrasing leftHand )      = renderLeftHands part leftHand
renderRightHand part ( Jete   phrasing leftHand )      = renderLeftHands part (zip bounceDur leftHand)

renderLeftHands :: Part -> [(Dur, LeftHand Pitch Str)] -> TremoloScore Dur MidiNote
renderLeftHands part  =  stretchTo 1 . concatSeq . map leftHands
    where
        leftHands (d, x)  =  stretch d $ renderLeftHand part x
\end{code}




Cues
------

TODO cleanup

\begin{code}
setMidiChannel :: MidiChannel -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiChannel c = Either.fmapEither f g
    where f = (\(MidiNote _ i p b n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote _ i p b n) -> MidiNote c i p b n)

setMidiInstrument :: MidiInstrument -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiInstrument i = Either.fmapEither f g
    where f = (\(MidiNote c _ p b n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote c _ p b n) -> MidiNote c i p b n)

setMidiBend :: MidiBend -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiBend b = Either.fmapEither f g
    where f = (\(MidiNote c i p _ n) -> MidiNote c i p b n)
          g = fmap (\(MidiNote c i p _ n) -> MidiNote c i p b n)

setMidiDynamic :: Dynamics -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiDynamic n = tmapEither f g
    where f = (\t (MidiNote c i p b _) -> MidiNote c i p b (renderDynamic n t))
          g = (\t x -> tmap (\t (MidiNote c i p b _) -> MidiNote c i p b (renderDynamic n t)) x)

setMidiDynamic' :: Part -> Dynamics -> TremoloScore Dur MidiNote -> TremoloScore Dur MidiNote
setMidiDynamic' r n = tmapEither f g
    where f = (\t (MidiNote c i p b _) -> MidiNote c i p b (renderDynamic' r n t))
          g = (\t x -> tmap (\t (MidiNote c i p b _) -> MidiNote c i p b (renderDynamic' r n t)) x)
\end{code}



Some constants used for the rendering of jeté strokes.

\begin{code}
bounceDur :: [Dur]
bounceDur  =  [ (2 ** (-0.9 * x)) / 6 | x <- [0, 0.1..1.2] ]

bounceVel :: [Double]
bounceVel  =  [ abs (1 - x) | x <- [0, 0.08..]]
\end{code}




Render each cue to a score of `MidiNote` elements. Each generated score has a duration of one, so
this function can be used with `>>=` to render a score of cues to Midi (see below.)

\begin{code}
renderCueToMidi :: Cue -> TremoloScore Dur MidiNote
renderCueToMidi cue =
    renderRest $ renderRightHand (cuePart cue) (cueTechnique cue)
    where
        channel     =  midiChannel cue
        instr       =  midiInstrument cue
        bend        =  midiBend cue

        renderRest  =  setMidiChannel channel
                    .  setMidiInstrument instr
                    .  setMidiBend bend
                    .  setMidiDynamic' (cuePart cue) (cueDynamics cue)
\end{code}

This instance makes it possible to use the `play` function on scores of cues:

\begin{code}
instance Render (Score Dur Cue) Midi where
    render  =  render
            .  restAfter 5
            .  renderTremoloEvents
            .  (>>= renderCueToMidi)

\end{code}

We can exploit the `renderCueToMidi` function to get a piano-roll style graphical
representation of the piece.

\begin{code}

class Plottable a where
    plot :: a -> IO ()

instance Plottable (Score Dur (Int, Int, Double)) where
    plot = draw . plotPitches
instance Plottable (Score Dur MidiNote) where
    plot = plot . (fmap (\x -> (midiNotePitch x, midiNoteVelocity x, midiNoteBend x)))
instance Plottable (Score Dur Cue) where
    plot = plot . renderTremoloEvents . (>>= renderCueToMidi)


plotPitches :: Score Dur (Int, Int, Double) -> Engraving
plotPitches = pad . mconcat . map (\(Event t d (x,y,b)) -> p t x b . s y $ l d) . toEvents
    where
        pad x = strutY 1 `above` x `above` strutY 1
        p t x b = moveSpacesRight (Spaces t) . moveHalfSpacesUp (HalfSpaces $ (fromIntegral x) + b)
        s v   = lineWidth 0 . fillColor black . opacity ((fromIntegral v + 40) / 127)
        l d   = alignL $ rect (convert space * d) (convert $ space/4)


\end{code}

\pagebreak


Notation
=================

The previous chapter we defined a rendering from the internal score representation to MIDI.
In this chapter we will define a similar functionality for music notation. We will use the
`Music.Notable` (which in turn depends on `Diagrams`) for the purposes of notation.
As this is an orchestral piece, our task is twofold: first we need to render all cues and
parts in to a single score, then we need to separate the cues into orchetral parts.


Rhytm and spacing
--------

`Music.Notable` use a simple hierarchical representation for graphical elements: each notation of a number of systems, which in
turn consists of staves, consisting of chords, consisting of notes. On each levels secondary objects such as bar numbers, clefs,
bar lines, accidentals and so on may be added. Each staff in a system is associated with a vertical space value, while each
object in a staff is associated with a horizontal space value.

In standard notation, horizontal spacing is not linear, but logarithmic (the space between half-notes is less than twice
the space of quarter notes, for example). As we use multiple tempos this may be confusing, as the proportions between durations
can not easily be seen from the high-level graphical structure. We therefore opt for a linear spacing instead.

\begin{code}
kHorizontalSpace :: Spaces
kHorizontalSpace = 7.5

timeToSpace :: NoteValue -> Spaces
timeToSpace = (* kHorizontalSpace) . Spaces
\end{code}


\begin{code}
notateTempo :: Tempo -> NonSpacedObject
notateTempo = StaffMetronomeMark (1/2) . toMetronomeScale . truncate

calculateTempo :: Score Dur Cue -> Score Dur Cue
calculateTempo = dmap (\d x -> setTempo (60 * (getDur . cueTechnique) x / d) x)
\end{code}


Dynamics
--------

\begin{code}
notateDynamic :: Dynamics -> NonSpacedObject
notateDynamic x
    | x `levelAt` 0 <= -0.8  =  StaffDynamic $ Notable.ppp
    | x `levelAt` 0 <= -0.6  =  StaffDynamic $ Notable.pp
    | x `levelAt` 0 <= -0.3  =  StaffDynamic $ Notable.p
    | x `levelAt` 0 <= 0.0   =  StaffDynamic $ Notable.mf
    | x `levelAt` 0 <= 0.25  =  StaffDynamic $ Notable.f
    | x `levelAt` 0 <= 0.5   =  StaffDynamic $ Notable.ff
    | otherwise              =  StaffDynamic $ Notable.fff
\end{code}


Pitch spelling
--------

We now have to pay the price for our simplistic pitch representation: we have no simple way
of distinguishing accidentals. Fortunately, the pitch material of this piece is not complicated,
and it makes sense to write every non-natural pitch as a sharp.

\begin{code}
toDiatonic :: Pitch -> (Pitch, Maybe Accidental)
toDiatonic x = (l + h, a)
    where
        l      =  7 * (x `div` 12)
        (h, a) =  sharpsOnly (x `mod` 12)

sharpsOnly 0  =  (0, Nothing)
sharpsOnly 1  =  (0, Just Sharp)
sharpsOnly 2  =  (1, Nothing)
sharpsOnly 3  =  (1, Just Sharp)
sharpsOnly 4  =  (2, Nothing)
sharpsOnly 5  =  (3, Nothing)
sharpsOnly 6  =  (3, Just Sharp)
sharpsOnly 7  =  (4, Nothing)
sharpsOnly 8  =  (4, Just Sharp)
sharpsOnly 9  =  (5, Nothing)
sharpsOnly 10 =  (5, Just Sharp)
sharpsOnly 11 =  (6, Nothing)

standardClef :: Part -> Clef
standardClef (Violin _)  = trebleClef
standardClef (Viola _)   = altoClef
standardClef (Cello _)   = bassClef
standardClef DoubleBass  = bassClef

indicatedPitch :: Clef -> (Pitch, Pitch)
indicatedPitch (GClef, -2) = (72, 1)
indicatedPitch (CClef,  0) = (60, 0)
indicatedPitch (FClef,  2) = (48, -1)

transposition :: Part -> Pitch
transposition (Violin _)  = 0
transposition (Viola _)   = 0
transposition (Cello _)   = 0
transposition DoubleBass  = (-12)

notatePitch :: Clef -> Pitch -> (HalfSpaces, Maybe Accidental)
notatePitch c x = (HalfSpaces . fromIntegral $ p + d', a)
    where
        (p, a)  = toDiatonic (x - d)
        (d, d') = indicatedPitch c
\end{code}


Rehearsal marks
--------

\begin{code}
notateRehearsal :: String -> NonSpacedObject
notateRehearsal = StaffRehearsal

extractRehearsalMarks :: Score Dur Cue -> [(Dur, Mark)]
extractRehearsalMarks = List.nubBy (testing fst) . concatMap (\(Event t d xs) -> zip (repeat t) xs) . toEvents . fmap cueMarks

addRehearsalMark :: Score Dur Cue -> Score Dur Cue
addRehearsalMark = mapFirstEvent (\(Cue p d n e m t) -> Cue p d n e (m ++ [RehearsalMark]) t)

testing f x y = f x == f y
\end{code}



Notating staves
--------

\begin{code}
staffHead :: Clef -> Staff
staffHead clef = trivial { spacedObjects = s }
    where
        s = [(kStaffSpaceBeforeClef, StaffClef clef)]

prependStaffHead :: Clef -> Staff -> Staff
prependStaffHead clef staff = staffHead clef `mappend` moveObjectsRight kStaffHeadWidth staff

kStaffHeadWidth       = 5   :: Spaces              
kStaffSpaceBeforeClef = 0.5 :: Spaces
kStaffHeadExtraSpace  = kStaffHeadWidth + kStaffSpaceBeforeClef


notateOpenStringPitch :: Part -> Str -> (HalfSpaces, Maybe Accidental)
notateOpenStringPitch r s = (p,a)
    where
        (p, a) = notatePitch c . (subtract t) $ openStringPitch r s
        c  = standardClef r
        t  = transposition r

notateNaturalHarmonicPitch :: Part -> Str -> Int -> (HalfSpaces, Maybe Accidental)
notateNaturalHarmonicPitch r s x = (p,a)
    where
        (p, a) = notatePitch c . (subtract t) $ naturalHarmonicWrittenPitch r s x
        c  = standardClef r
        t  = transposition r

notateStoppedStringPitch :: Part -> Pitch -> (HalfSpaces, Maybe Accidental)
notateStoppedStringPitch r x = (p,a)
    where
        (p,a) = notatePitch c . (subtract t) $ x
        c  = standardClef r
        t  = transposition r


pitchPosition :: Part -> Technique -> HalfSpaces
pitchPosition r (Single _ (OpenString s))        = fst $ notateOpenStringPitch r s
pitchPosition r (Single _ (NaturalHarmonic x s)) = fst $ notateNaturalHarmonicPitch r s x
pitchPosition r (Single _ (StoppedString x s))   = fst $ notateStoppedStringPitch r x
pitchPosition r _                                = 0

notateOpenString :: Part -> NoteValue -> Str -> (Chord, Maybe NonSpacedObject)
notateOpenString r nv s = (ch, Nothing)
    where
        ch = trivial { dots = dotsFromNoteValue nv,
                       notes = [Note p nh a] }
        nh = noteHeadFromNoteValue nv
        (p,a) = notateOpenStringPitch r s

notateNaturalHarmonic :: Part -> NoteValue -> Str -> Pitch -> (Chord, Maybe NonSpacedObject)
notateNaturalHarmonic r nv s x = (ch, Nothing{-Just strText-})
    where
        ch = trivial { dots = dotsFromNoteValue nv,
                       notes = [Note p DiamondNoteHead a] }
        nh = noteHeadFromNoteValue nv
        (p,a) = notateNaturalHarmonicPitch r s x  
        -- strText = StaffInstruction $ show s

notateStoppedString :: Part -> NoteValue -> Str -> Pitch -> (Chord, Maybe NonSpacedObject)
notateStoppedString r nv s x = (ch, Nothing)
    where
        ch = trivial { dots = dotsFromNoteValue nv,
                  notes = [Note p nh a] }
        nh = noteHeadFromNoteValue nv
        (p,a) = notateStoppedStringPitch r x

-- TODO trem, gliss
notateLeftHand :: Part -> NoteValue -> LeftHand Pitch Str -> (Chord, Maybe NonSpacedObject)
notateLeftHand r nv ( OpenString           s )   =  notateOpenString r nv s
notateLeftHand r nv ( NaturalHarmonic      x s ) =  notateNaturalHarmonic r nv s x
notateLeftHand r nv ( StoppedString        x s ) =  notateStoppedString r nv s x

-- TODO pizz, jete
notateRightHand :: Part -> Technique -> ([(Spaces, Chord)], [NonSpacedObject])
notateRightHand r ( Single _ x )   =  ([(0, ch)], maybeToList nsp) where (ch, nsp) = notateLeftHand r 1 x
notateRightHand r ( Phrase _ xs )  =  moveMaybe . snd $ List.mapAccumL (\t (d, x) -> (t + timeToSpace d, (t, notateLeftHand r (d/2) x))) 0 xs





kNoteHeadOffset :: Spaces
kNoteHeadOffset = 1.8

kSustainLineOffset :: Spaces
kSustainLineOffset = 2.2

notateCue :: Cue -> Staff
notateCue cue = trivial { spacedObjects = sN, nonSpacedObjects = nsN }
    where
        sN = barLineN ++ chordsN ++ sustainLineN

        barLineN = if phr then [(0, StaffBarLine)]
                          else [(0, StaffNothing)]

        chordsN            =  fmap (\(t,x) -> (t * scale + kNoteHeadOffset, StaffChord x)) chordsN'
        (chordsN', nspN )  =  notateRightHand (cuePart cue) (cueTechnique cue)

        sustainLineN  =  x `emptyWhen` phr
            where
                x    =  [(pos, obj)]
                pos  =  kNoteHeadOffset + kSustainLineOffset
                yPos =  pitchPosition (cuePart cue) (cueTechnique cue)
                obj  =  StaffSustainLine (yPos, scale * kHorizontalSpace - (kNoteHeadOffset + 4.4))


        nsN = {-rehearsalN ++ -}tempoN ++ dynamicN ++ map (\x -> ([0], x)) nspN

        -- rehearsalN = [([1], notateRehearsal $ "A")] `nonEmptyWhen` True
        tempoN     = [([1], notateTempo . cueTempo $ cue)] `nonEmptyWhen` phr
        dynamicN   = [([1], notateDynamic . cueDynamics $ cue)]

        scale = Spaces (60 / (cueTempo cue))
        phr = isPhrase $ cueTechnique cue
               

notateRehearsalMarks :: Score Dur Cue -> Staff
notateRehearsalMarks score = trivial { spacedObjects = sN, nonSpacedObjects = nsN }
    where
        (sN, nsN) = unzip . concatMap (\((t, _), i) -> [((timeToSpace t, StaffNothing), ([i], notateRehearsal $ mark i))]) $ zip marks [0..]
        marks = extractRehearsalMarks score
        mark i = [chr (i + 65)]


\end{code}

Removing redundant information
--------

As we notated each cue separately, information such as dynamics and metronome marks are goin to be repeated each time
a new cue starts. However, in standard notation, identical tempi marks are never repeated. We therefore define a
function to remove reduncant marks from a staff.

TODO cleanup

\begin{code}
removeRedundantMarks :: Staff -> Staff
removeRedundantMarks = removeRedundantMarks'
-- removeRedundantMarks = id

removeRedundantMarks' (Staff o s ns) = Staff o s (snd $ List.concatMapAccumL f z ns)
    where
        z :: (Maybe Notable.Dynamic, Maybe (NoteValue, BeatsPerMinute))
        z = (Nothing, Nothing)

        f (Nothing, t) (p, (StaffDynamic x)) = ((Just x, t), [(p, StaffDynamic x)])
        f (Just x', t) (p, (StaffDynamic x)) = if x == x' then ((Just x, t), []) else ((Just x, t), [(p, StaffDynamic x)])

        f (n, Nothing)       (p, (StaffMetronomeMark x y)) = ((n, Just (x,y)), [(p, StaffMetronomeMark x y)])
        f (n, Just (x', y')) (p, (StaffMetronomeMark x y)) = if x == x' && y == y' then ((n, Just (x,y)), [])
                                                                else ((n, Just (x,y)), [(p, StaffMetronomeMark x y)])

        f z x = (z, [x])

\end{code}


Notating parts
----------

\begin{code}
notatePart :: Score Dur Cue -> Staff
notatePart score = removeRedundantMarks . mconcat . fmap notateCueEvent . toEvents $ score
    where
        notateCueEvent (Event t d x) = moveObjectsRight (timeToSpace t) $ notateCue x


kDefaultClef = trebleClef

-- guess which clef to use
scoreClef :: Time t => Score t Cue -> Clef
scoreClef = maybe kDefaultClef (standardClef . cuePart) . firstEvent

scorePartName :: Time t => Score t Cue -> String
scorePartName = maybe "" (partName . cuePart) . firstEvent

\end{code}



Score and parts
--------

The simplest way to engrave a score is in panorama form.

\begin{code}
engravePanorama :: Score Dur Cue -> [Score Dur Cue] -> [Engraving]
engravePanorama global =
    fmap (addVerticalSpace . engraveStaff) . equalizeStaffWidth . addRehearsals . fmap notatePart
    where                 
        addVerticalSpace     = (<> spaceY 4)            
        addRehearsals (x:xs) = (notateRehearsalMarks global `mappend` x):xs
\end{code}



Engraving of part books.

\begin{code}

kPartPageWidth           = 140 :: Spaces
kPartSystemsPerPage      = 13
kPartSpaceBetweenSystems = 8

kPartPageDimensions = r2 (210, 297)             
kPartPageMarigins   = r2 (15, -20)
kPartPageScaling    = 4.8

engravePartLines :: Score Dur Cue -> [Engraving]
engravePartLines score = engravePartLines' clef score
    where
        clef = scoreClef score
        
engravePartLines' :: Clef -> Score Dur Cue -> [Engraving]
engravePartLines' clef = fmap engraveStaff . fmap addStaffHead . divideStaff kPartPageWidth . addRehearsals . notatePart
    where   
        addStaffHead x = prependStaffHead clef x
                                         
        addRehearsals :: Staff -> Staff    
        addRehearsals = (notateRehearsalMarks score `mappend`)

engravePartPages :: Score Dur Cue -> [Engraving]
engravePartPages score = engravePartPages' name score
    where
        name = scorePartName score

engravePartPages' :: String -> Score Dur Cue -> [Engraving]
engravePartPages' partName = addPageHeaders . map partPage . map catDown . map (map addSystemSpacer) . List.divide kPartSystemsPerPage . engravePartLines
    where              
        addSystemSpacer :: Engraving -> Engraving
        addSystemSpacer   = (<> spaceY (kPartSpaceBetweenSystems / 2))  
        
        addPageHeaders :: [Engraving] -> [Engraving]
        addPageHeaders xs = map (uncurry $ addPageHeader "Passager" partName) $ zip [1..] xs

        addPageHeader :: String -> String -> Int -> Engraving -> Engraving
        addPageHeader name partName pageNum = (pageHeader name partName pageNum <>)

        pageHeader :: String -> String -> Int -> Engraving
        pageHeader name partName pageNum = moveSpacesUp 5 $ mempty
            <> (                        alignR  . scale 4 $ engraveText partName)
            <> (moveSpacesRight w     . alignL  . scale 4 $ engraveText name)
            <> (moveSpacesRight (w/2) . centerX . scale 3 $ engraveText (show pageNum))
            where
                w = kPartPageWidth * kPartPageScaling
        
partPage :: Engraving -> Engraving
partPage = (<> page) . alignTL . scale kPartPageScaling . freeze
    where
        page = st . moveOriginBy kPartPageMarigins . alignTL $ r (getX kPartPageDimensions) (getY kPartPageDimensions)
        r    = spaceRect   
        st   = id
        -- r    = rect
        -- st   = fillColor lightyellow

data PartBook =
    PartBook
    {
        partBookPart :: Part,
        partBookScore :: Score Dur Cue
    }

partBookFilePath :: PartBook -> FilePath
partBookFilePath (PartBook p s) = {-toLowerCase . filter isAlphaNum $-} partName p

engravePartBook :: PartBook -> [Engraving]
engravePartBook = engravePartPages . partBookScore

writePartBook :: FilePath -> PartBook -> IO ()
writePartBook path book =
    let (files, action) = writePartBookPages path book in
        do  action
            joinPdfs files (path ++ partBookFilePath book ++ ".pdf")
            threadDelay 2000000
            removeFiles files

writePartBookPages :: FilePath -> PartBook -> ([FilePath], IO ())
writePartBookPages path book = (take (length pages) names, writeMultipleGraphics (zip names pages))
    where
        name  = path ++ partBookFilePath book
        names = map (\i -> name ++ "_p" ++ show i ++ ".pdf") [1..]
        pages = map Graphic $ engravePartBook book
\end{code}




Engraving of the score book.

\begin{code}

kScorePageWidth           = 340 :: Spaces
kScoreSpaceBetweenSystems = 10

kScorePageDimensions = r2 (420, 297)             
kScorePageMarigins   = r2 (10, -50)
kScorePageScaling    = 4.5

equalizeStaffWidth :: [Staff] -> [Staff]
equalizeStaffWidth xs = map (f . setMinWidth m) xs
    where                     
        f = insertSpacedObject m StaffNothing
        m = List.maximumWith 0 . map staffWidth $ xs

engraveLeftLine :: Int -> Engraving
engraveLeftLine numParts = p $ vrule (kScoreSpaceBetweenSystems * fromIntegral (numParts - 1) / 2 + (convert $ Spaces 4))
    where
        p = translateY (convert $ Spaces 2) . alignT

engraveScorePages :: [Part] -> Score Dur Cue -> [Engraving]
engraveScorePages parts score = 
          map scorePage
        . map (<> leftLineE)
        . map catDown 
        . map (map addSystemSpacer) 
        . map (map engraveStaff) 
        . List.transpose 
        . fmap (divideStaff kScorePageWidth)            
        . addRehearsals 
        . equalizeStaffWidth
        $ partsN
    where      
        leftLineE = engraveLeftLine (length parts)

        addSystemSpacer :: Engraving -> Engraving
        addSystemSpacer   = (<> spaceY (kScoreSpaceBetweenSystems / 2))  
             
        partsN :: [Staff]
        partsN       =  fmap notatePart $ extractParts parts score
        
        rehearsalsN :: Staff
        rehearsalsN  =  notateRehearsalMarks score 

        addStaffHead :: [[Staff]] -> [[Staff]]
        addStaffHead = id                                  
                
        addRehearsals :: [Staff] -> [Staff]
        addRehearsals (x:xs) = (rehearsalsN `mappend` x):xs


        
scorePage :: Engraving -> Engraving
scorePage = (<> page) . alignTL . scale kScorePageScaling . freeze
    where
        page = st . moveOriginBy kScorePageMarigins . alignTL $ r (getX kScorePageDimensions) (getY kScorePageDimensions)
        r    = spaceRect   
        st   = fillColor lightblue

data ScoreBook =
    ScoreBook
    {                        
        scoreBookParts :: [Part],
        scoreBookScore :: Score Dur Cue
    }

scoreBookFilePath :: ScoreBook -> FilePath
scoreBookFilePath (ScoreBook p s) = "Partitur"

engraveScoreBook :: ScoreBook -> [Engraving]
engraveScoreBook book = engraveScorePages (scoreBookParts book) (scoreBookScore book)

writeScoreBook' :: FilePath -> ScoreBook -> IO ()
writeScoreBook' path book =
    let (files, action) = writeScoreBookPages path book in
        do  action
            joinPdfs files (path ++ scoreBookFilePath book ++ ".pdf")
            threadDelay 2000000
            removeFiles files

writeScoreBookPages :: FilePath -> ScoreBook -> ([FilePath], IO ())
writeScoreBookPages path book = (take (length pages) names, writeMultipleGraphics' (getY kScorePageDimensions) (zip names pages))
    where
        name  = path ++ scoreBookFilePath book
        names = map (\i -> name ++ "_p" ++ show i ++ ".pdf") [1..]
        pages = map Graphic $ engraveScoreBook book
\end{code}           
                    


These functions extracts parts from a score, based on the `cuePart` attribute:

\begin{code}
extractPart :: Part -> Score Dur Cue -> Score Dur Cue
extractPart part = filterEvents (cuePart `equals` part)

extractParts :: [Part] -> Score Dur Cue -> [Score Dur Cue]
extractParts parts score = map (flip extractPart $ score) parts
\end{code}





Finally, we define the panorama, score book and part book engravings, based on the `score` and `ensemble` variables.

\begin{code}   
ensemble' = ensemble
-- ensemble' = (take 2 violins) ++ cellos
    
-- | All parts, generated from 'score'.
parts :: [Score Dur Cue]
parts = extractParts ensemble' (calculateTempo score)

partBooks :: [PartBook]
partBooks = fmap (\(p,s) -> PartBook p s) $ zip ensemble' parts

scoreBook :: ScoreBook
scoreBook = ScoreBook ensemble' (calculateTempo score)

panoramaParts :: [Engraving]
panoramaParts = engravePanorama score parts

panoramaScore :: Engraving
panoramaScore = m <> sp
    where
        m  = foldr above mempty $ panoramaParts
        sp = moveOriginBy (r2(0,-7)) . alignTL $ spaceY 45

showPanoramaScore :: IO ()
showPanoramaScore = draw panoramaScore

writePartBooks :: IO ()
writePartBooks = sequence_ $ map (writePartBook "parts/") partBooks

writeScoreBook :: IO ()
writeScoreBook = writeScoreBook' "parts/" scoreBook

writeAll :: IO ()
writeAll = do
    writeScoreBook
    writePartBooks
\end{code}


Finally, we add instances to let us use the `draw` function on chords, staves and engravings.

\begin{code}
instance Render Chord Graphic where
    render = Graphic . engraveChord

instance Render Staff Graphic where
    render = Graphic . engraveStaff

instance Render Engraving Graphic where
    render = Graphic
\end{code}





Musical material
==========

Pitches
----------

The pitch material is based on a 15-tone symmetric scale. The lower half is mixolydian
and the upper half is aeolian.

We will represent melodies in a relative fashion, using 0 to represent a reference pitch.
This simplifies inversion and similar techniques. We use A3 as the reference pitch
(corresponding to step 0). The `tonality` function applies the major-minor scale at
the reference pitch, so pitch operations applied *before* this function are tonal, while
operations applied *after* it are chromatic.

\begin{code}
majMinScale = Scale $ getScale lower ++ getScale upper
    where
        lower = retrograde . invert $ naturalMinor
        upper = Scale . tail . getScale $ naturalMinor

tonality :: PitchFunctor f => f -> f
tonality = mapPitch $ offset . scale . tonic
    where
        tonic  = (+ 7)
        scale  = (majMinScale `step`)
        offset = (+ 57)

\end{code}

The `tonalSeq` function generates a binary musical sequence, in which the second operand is transposed
the given amount of steps. The `tonalConcat` function is the same on higher arities.

\begin{code}
tonalSeq :: (Time t, PitchFunctor a) => Pitch -> Score t a -> Score t a -> Score t a
tonalConcat :: (Time t, PitchFunctor a) => Pitch -> [Score t a] -> Score t a

tonalSeq    p x y  =  x >>> mapPitch (+ p) y
tonalConcat p      =  List.foldr (tonalSeq p) instant
\end{code}

The `fifthUp`, `fifthDown` etc are handy shortcuts for (chromatic) transposition.

\begin{code}
duodecDown, octaveDown, fifthDown :: PitchFunctor a => a -> a
fifthUp, octaveUp, duodecUp       :: PitchFunctor a => a -> a

duodecDown  =  mapPitch (+ (-19))
octaveDown  =  mapPitch (+ (-12))
fifthDown   =  mapPitch (+ (-7))
fifthUp     =  mapPitch (+ 7)
octaveUp    =  mapPitch (+ 12)
duodecUp    =  mapPitch (+ 19)

transposeUp x   = mapPitch (+ x)
transposeDown x = mapPitch (+ (-x))
\end{code}



Melody
----------

The melody is based on short melodic patterns. We represent these as duration–pitch
pairs and provide the `patternMelody` function to convert them into a melody. The
duration is in half-seconds but it can of course be scaled to any tempo.

We use three patterns are based on a simple upward-moving stepwise gesture (0-3).
To get downward motion, we use the invert function.


\begin{code}
type Pattern =  [(Dur, Pitch)]

instance PitchFunctor Pattern where
    mapPitch f xs = zip ds (fmap f ps)
        where (ds, ps) = unzip xs

pattern :: Int -> Pattern
pattern n
    | n < 0      =  invert $ patterns !! (negate n)
    | otherwise  =  patterns !! n
    where
        patterns =
            [
            zip [ 3, 3 ]
                [ 0, 1 ],
            zip [ 1, 1, 1, 1, 3, 3 ]
                [ 0, 1, 1, 2, 0, 1 ],
            zip [ 1, 1, 1, 2, 1, 3, 3 ]
                [ 0, 1, 1, 2, 3, 0, 1 ]
            ]

patternMelody :: Pattern -> Score Dur Cue
patternMelody x = stretch (scaling x / 2) . stoppedStrings $ x
    where
        scaling = sum . map fst

patternMelodyFrom :: Int -> Score Dur Cue
patternMelodyFrom = patternMelody . pattern

\end{code}

Arbitrarily complex melodies can be formed by combinations of the three basic patterns.

\begin{code}
patternSequence :: Pitch -> [Pattern] -> Score Dur Cue
patternSequence p = tonalConcat p . fmap patternMelody

patternSequenceFrom :: Pitch -> [Int] -> Score Dur Cue
patternSequenceFrom p = patternSequence p . fmap pattern
\end{code}


Imitation
----------

\begin{code}
type Sc = Score Dur Cue
type Tr = Sc -> Sc

canon :: RandomGen r => r -> Int -> Pitch -> [(Part, Tr)] -> Sc
canon r i p ts = canon' False r i p (map (\(p,t) -> setPart p . t) ts)

invertedCanon :: RandomGen r => r -> Int -> Pitch -> [(Part, Tr)] -> Sc
invertedCanon r i p ts = canon' False r i p (map (\(p,t) -> setPart p . t) ts)

canon' :: RandomGen r => Bool -> r -> Int -> Pitch -> [Tr] -> Sc
canon' inv r len step tr =
    concatPar $ fmap createPart (zip tr rs)
    where
        createPart (f,r) = f . tonality . i . s $ (take len $ randomPatterns r)
        s   =  patternSequenceFrom step
        i   =  invert `onlyWhen` inv
        rs  =  splitted r
\end{code}


Score
----------

\begin{code}

-- instant >>> ||| concatSeq, concatPar
-- reverse loop
-- duration stretch compress stretchTo
-- note rest delay
-- split before after

-- sustain prolong anticipate


-- Misc notes

dbFs  = setPart DoubleBass . stretch 4 $ naturalHarmonic III 4
dbB   = setPart DoubleBass . stretch 4 $ naturalHarmonic IV 4

dbE   = setPart DoubleBass . stretch 4 $ openString I
dbA   = setPart DoubleBass . stretch 4 $ openString II
dbD   = setPart DoubleBass . stretch 4 $ openString III
dbG   = setPart DoubleBass . stretch 4 $ openString IV

dbE'  = setPart DoubleBass . stretch 4 $ naturalHarmonic I  1
dbA'  = setPart DoubleBass . stretch 4 $ naturalHarmonic II 1
dbD'  = setPart DoubleBass . stretch 4 $ naturalHarmonic III 1
dbG'  = setPart DoubleBass . stretch 4 $ naturalHarmonic IV 1

dbE'' = setPart DoubleBass . stretch 4 $ naturalHarmonic I  3
dbA'' = setPart DoubleBass . stretch 4 $ naturalHarmonic II 3
dbD'' = setPart DoubleBass . stretch 4 $ naturalHarmonic III 3
dbG'' = setPart DoubleBass . stretch 4 $ naturalHarmonic IV 3


-- Intro/midtro/outro

introHarm :: Int -> Score Dur Cue
introHarm sect = stretch 3 $ loop x
    where
        x  =  stretch 2.4 a >>> stretch 2.2 g >>> stretch 3.4 a >>> rest 1
        g  =  setPart (Viola sect) $ naturalHarmonic II 1
        a  =  setPart (Cello sect) $ naturalHarmonic IV 1

midtroHarm1 :: Int -> Score Dur Cue
midtroHarm1 sect = stretch 2.2 $ loop x
    where
        x  =  stretch 2.5 a >>> rest 1 >>> stretch 3.1 a >>> rest 1
        a  =  setPart (Cello sect) $ openString IV

midtroHarm2 :: Int -> Score Dur Cue
midtroHarm2 sect = stretch 3 $ loop x
    where
        x = stretch 2.4 a >>> stretch 2.2 g >>> stretch 3.4 a >>> rest 1
        a  = setPart (Violin $ sect + 2) $ naturalHarmonic III 1
        g  = setPart (Violin $ sect)     $ naturalHarmonic I 3

outtroHarm :: Int -> Int -> Score Dur Cue
outtroHarm vs cs = stretch 2 $ loop x
    where
        x  =  stretch 2.2 g >>> stretch 3.4 a >>> stretch 2.8 g >>> rest 1
        g  =  setPart (Violin vs) $ openString I
        a  =  setPart (Cello cs)  $ openString IV

outtroHarm' :: Int -> Int -> Score Dur Cue
outtroHarm' vs cs = stretch 2 $ loop x
    where
        x  =  stretch 2.2 a >>> stretch 3.4 g >>> stretch 2.8 a >>> rest 1
        a  =  setPart (Violin vs) $ openString III
        g  =  setPart (Viola cs)  $ openString II

outtroHarm'' :: Int -> Int -> Score Dur Cue
outtroHarm'' vs cs = stretch 2 $ loop x
    where
        x  =  stretch 2.2 a >>> stretch 3.4 g >>> stretch 2.8 a >>> rest 1
        g  =  setPart (Viola cs)  $ openString II
        a  =  setPart (Cello cs)  $ openString IV


intro1 =                     setDynamics pp $ instant
    ||| (           before 30 $ introHarm 1)
    ||| (delay 15 . before 30 $ introHarm 2) ||| (delay 25 . stretch 5 $ dbFs)
    ||| (delay 35 . before 35 $ introHarm 1)
    ||| (delay 50 . before 35 $ introHarm 2) ||| (delay 60 . stretch 5 $ dbB)

midtro1 = addRehearsalMark . setDynamics mf $ instant
    ||| (           before 30 $ midtroHarm1 1)
    ||| (delay 15 . before 30 $ midtroHarm1 2)
    ||| (delay 6  . stretch 3 $ dbA'')
    ||| (delay 24 . stretch 3 $ dbA'')

midtro2 = addRehearsalMark . setDynamics pp $ instant
    ||| (          before 40 $ midtroHarm2 1)
    ||| (delay 0 . before 40 $ midtroHarm2 2)

outro1 = outro1' `prolong` outro1bass

outro1' = addRehearsalMark . setDynamics p $ stretch 1.5 $ instant
    ||| (           stretch 1   . before 40 $ outtroHarm  1 1)
    ||| (delay 2  . stretch 1.2 . before 40 $ outtroHarm  2 2)
    ||| (delay 10 . stretch 1.3 . before 30 $ outtroHarm' 3 1)
    ||| (delay 12 . stretch 1.4 . before 50 $ outtroHarm' 4 2)
    ||| (delay 50 . stretch 1.4 . before 50 $ outtroHarm'' 1 1)
    ||| (delay 50 . stretch 1.4 . before 50 $ outtroHarm'' 2 2)

outro1bass = setDynamics p . loop $
    rest 20 >>> stretch 3 dbA'' >>> rest 10 >>> stretch 3 dbG'

-- Presentation and canons

present1 = addRehearsalMark $ instant
    ||| (          concatSeq . List.intersperse (rest 10) . map octaveUp $ map (present' Viola) [0,1,2])
    ||| (delay 15 . concatSeq . List.intersperse (rest 10) . map id       $ map (present' Cello) [0,1])


present' instr pattern = setDynamics pp . stretch 2.2 $ b ||| delay 5 a
    where
        a = (id          . tonality . setPart (instr 1)  $ patternMelodyFrom pattern)
        b = (stretch 1.1 . tonality . setPart (instr 2)  $ patternMelodyFrom pattern)


present2 = addRehearsalMark . setDynamics pp . stretch 2.2 . setDynamics mf $ instant
    ||| (          stretch 1   . id . tonality . setPart (Cello 2) $ patternSequenceFrom 0 $ [0,1,0,1,0])
    ||| (delay 6 . stretch 1.3 . id . tonality . setPart (Cello 1) $ patternSequenceFrom 0 $ [1,0,1,0,1])

present3 = setDynamics p . stretch 1.6 . reverse $
    canon rand 8 0 (zip ps (ss `compose` ts))
    where
        ps = ([Violin 3, Violin 4, Viola 1, Viola 2])
        ss = map delay [0,3..] `compose` map stretch [1,1.16..]
        ts = map transposeUp [7,7,0,0]

present3high = addRehearsalMark . setDynamics p . stretch 1.6 . reverse $
    canon rand 7 0 (zip ps (ss `compose` ts))
    where
        ps = ([Violin 1, Violin 2])
        ss = map delay [0,3..] `compose` map stretch [1,1.2..]
        ts = map transposeUp [12,12]

midCanon = (addRehearsalMark $ reverse midCanon') >>> (addRehearsalMark $ stretch 0.8 midCanon')

-- midCanon' = setDynamics mf . compress 1.1 $
--     canon rand 4 0 (zip ps ts)
--     where
--         ps = violins ++ violas
--         ts = map stretch [2.1,2.2,2.5,2.9,3.5,4.1] `compose` map transposeUp [12,12,7,7,0,0]

midCanon' = setDynamics mf . compress 1.1 $ instant
    ||| (stretch 4.1 . id       . tonality . setPart (Viola  2) . patternSequenceFrom 0 $ [1,2,1,0,0,2])
    ||| (stretch 2.1 . octaveUp . tonality . setPart (Violin 1) . patternSequenceFrom 0 $ [0,2,0,0,1,2])
    ||| (stretch 2.2 . octaveUp . tonality . setPart (Violin 2) . patternSequenceFrom 0 $ [1,2,0,0,1,2])
    ||| (stretch 2.5 . fifthUp  . tonality . setPart (Violin 3) . patternSequenceFrom 0 $ [2,0,0,1,0,2])
    ||| (stretch 2.9 . fifthUp  . tonality . setPart (Violin 4) . patternSequenceFrom 0 $ [0,2,1,1,1,2])
    ||| (stretch 3.5 . id       . tonality . setPart (Viola  1) . patternSequenceFrom 0 $ [2,2,1,2,1,0])

midCanonCellos = setDynamics mf $ instant
    ||| (delay 0  . stretch 12 . tonality  . setPart (Cello 1) . concatSeq . List.intersperse (rest 0.3) $ map stoppedString [0,4,4,0,4,0,0,4,0,4,0,4])
    ||| (delay 10 . stretch 14 . tonality . setPart (Cello 2) . concatSeq . List.intersperse (rest 0.3) $ map stoppedString [-3,0,0,-3,0,-3,-3,0,-3,0])

canon2upper = addRehearsalMark . setDynamics pp . reverse $ instant
    ||| (delay 25  . stretch 2.4  . duodecUp . tonality . setPart (Violin 1) . invert . patternSequenceFrom 0 $ [2,0,0,1,1,2])
    ||| (delay 20  . stretch 2.3  . duodecUp . tonality . setPart (Violin 2) . invert . patternSequenceFrom 0 $ [0,0,1,1,2,2])
    ||| (delay 15  . stretch 2.2  . duodecUp . tonality . setPart (Violin 3) . invert . patternSequenceFrom 0 $ [0,1,1,0,2,1])
    ||| (delay 10  . stretch 2.1  . duodecUp . tonality . setPart (Violin 4) . invert . patternSequenceFrom 0 $ [1,0,2,2,1,0])
    ||| (delay 5   . stretch 2.0  . octaveUp . tonality . setPart (Viola  1) . invert . patternSequenceFrom 0 $ [1,2,1,1,0,1])
    ||| (delay 0   . stretch 1.8  . octaveUp . tonality . setPart (Viola  2) . invert . patternSequenceFrom 0 $ [0,1,1,2,2,0])

canon2lower = addRehearsalMark . setDynamics pp $ instant
    ||| (           stretch 2.4 . id  . tonality . setPart (Cello 1) . invert . patternSequenceFrom 1 $ [2,1,2,1,0])
    ||| (delay 13 . stretch 2.3 . id  . tonality . setPart (Cello 2) . invert . patternSequenceFrom 1 $ [2,1,2,2,1])

finalCanon = addRehearsalMark . setDynamics f . compress 1.1 $ instant
    ||| (stretch 2    . duodecUp . tonality . setPart (Violin 1) $ upDownSequence 1 $ [0,2,2,2,1])
    ||| (stretch 2.2  . duodecUp . tonality . setPart (Violin 2) $ upDownSequence 1 $ [1,2,2,1,0])
    ||| (stretch 2.35 . octaveUp . tonality . setPart (Violin 3) $ upDownSequence 1 $ [1,2,0,2,1])
    ||| (stretch 2.5  . octaveUp . tonality . setPart (Violin 4) $ upDownSequence 1 $ [1,2,2,1,1])
    ||| (stretch 2.7  . fifthUp  . tonality . setPart (Viola 1)  $ upDownSequence 1 $ [2,1,2,1,0])
    ||| (stretch 3.1  . fifthUp  . tonality . setPart (Viola 2)  $ upDownSequence 1 $ [1,2,1,2,0])

upDownSequence x ps = patternSequenceFrom 1 ps >>> (mapPitch (+ 3) $ patternSequenceFrom (-1) ps)

finalCanonBass = setDynamics f . compress 1.1 $ instant
    --  ||| (concatSeq $ map (\x -> stretch 20 . setPart (Cello 1)  $ stoppedString x) $ take 3 [57,55,54,52,50])
    --  ||| (concatSeq $ map (\x -> stretch 30 . setPart (Cello 2)  $ stoppedString x) $ take 2 [54,52,50,49,48])
    ||| (concatSeq $ map (\x -> stretch 40 . setPart DoubleBass $ openString x)    $ [IV{-,III-}])


-- Harmonics

harmPatterns :: [[Int]]
harmPatterns = harmPatterns' 3
    where
        harmPattern = [-1,0,0,1] ++ fmap (+ 2) harmPattern
        harmPatterns' x =
            [ take x (drop 2 harmPattern),
              take x (drop 2 harmPattern),
              take (x + 2) harmPattern ] ++ harmPatterns' (x + 2)

harmDurations :: [Dur]
harmDurations = cycle [3,3,5]

-- works with the first 6 harmPatterns
playHarm :: Part -> Int -> Score Dur Cue
playHarm (Violin _) (-1) = setPart (Violin 1) $ naturalHarmonic I 4
playHarm (Violin _)   0  = setPart (Violin 1) $ naturalHarmonic II 1
playHarm (Violin _)   1  = setPart (Violin 1) $ naturalHarmonic II 2
playHarm (Violin _)   2  = setPart (Violin 1) $ naturalHarmonic II 3
playHarm (Viola _)  (-1) = setPart (Viola  1) $ naturalHarmonic II 1
playHarm (Viola _)    0  = setPart (Viola  1) $ naturalHarmonic III 1
playHarm (Viola _)    1  = setPart (Viola  1) $ naturalHarmonic III 2
playHarm (Viola _)    2  = setPart (Viola  1) $ naturalHarmonic III 3
playHarm (Cello _)  (-1) = setPart (Cello  1) $ naturalHarmonic III 1
playHarm (Cello _)    0  = setPart (Cello  1) $ naturalHarmonic IV 1
playHarm (Cello _)    1  = setPart (Cello  1) $ naturalHarmonic IV 2
playHarm (Cello _)    2  = setPart (Cello  1) $ naturalHarmonic IV 3

playOpen :: Part -> Int -> Score Dur Cue
playOpen (Violin _) (-1) = setPart (Violin 1) $ openString I
playOpen (Violin _)   0  = setPart (Violin 1) $ openString I
playOpen (Violin _)   1  = setPart (Violin 1) $ openString III
playOpen (Violin _)   2  = setPart (Violin 1) $ openString IV
playOpen (Viola _)  (-1) = setPart (Viola  1) $ openString II
playOpen (Viola _)    0  = setPart (Viola  1) $ openString II
playOpen (Viola _)    1  = setPart (Viola  1) $ openString III
playOpen (Viola _)    2  = setPart (Viola  1) $ openString IV
playOpen (Cello _)  (-1) = setPart (Cello  1) $ openString II
playOpen (Cello _)    0  = setPart (Cello  1) $ openString II
playOpen (Cello _)    1  = setPart (Cello  1) $ openString III
playOpen (Cello _)    2  = setPart (Cello  1) $ openString IV


harms :: Int -> Part -> Score Dur Cue
harms n part =
    concatSeq (List.intersperse (rest 5) notes)
    where
        notes = map (\(t, p) -> stretch t $ playHarm part p) (zip harmDurations patterns)
        patterns :: [Int]
        patterns = harmPatterns !! (n `mod` 6)

opens :: Int -> Part -> Score Dur Cue
opens n part =
    concatSeq (List.intersperse (rest 5) notes)
    where
        notes = map (\(t, p) -> stretch t $ playOpen part p) (zip harmDurations patterns)
        patterns :: [Int]
        patterns = harmPatterns !! (n `mod` 6)


harms1 = setDynamics pp . stretch 1.8 $ instant
    ||| (delay 0  . stretch 1.4  . setPart (Violin 4) $ harms 0 (Violin 1))
    ||| (delay 3  . stretch 1.3  . setPart (Violin 3) $ harms 1 (Violin 1))
    ||| (delay 6  . stretch 1.2  . setPart (Violin 2) $ harms 1 (Violin 1))
    ||| (delay 9  . stretch 1.1  . setPart (Violin 1) $ harms 2 (Violin 1))

harms2 = setDynamics pp . stretch 1 . reverse $ instant
    ||| (delay 0  . stretch 1    . setPart (Viola 2)  $ harms 0 (Viola 1))
    ||| (delay 1 . stretch 1.2  . setPart (Viola 1)  $ harms  0 (Viola 1))

harms3 = setDynamics pp . stretch 1 . reverse $ instant
    ||| (delay 7  . stretch 1    . setPart (Cello 2)  $ harms 3 (Cello 1))
    ||| (delay 11 . stretch 1.2  . setPart (Cello 1)  $ harms 3 (Cello 1))


harms5 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1    . setPart (Viola 2)  $ harms 0 (Viola 1))
    ||| (delay 3 . stretch 1.2  . setPart (Viola 1)  $ harms 0 (Viola 1))

harms6 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1   . setPart (Violin 3)  $ harms 0 (Violin 1))
    ||| (delay 3 . stretch 1.2  . setPart (Violin 4)  $ harms 0 (Violin 1))


opens5 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1    . setPart (Viola 2)  $ opens 0 (Viola 1))
    ||| (delay 3 . stretch 1.2  . setPart (Viola 1)  $ opens 0 (Viola 1))

opens6 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1   . setPart (Violin 3)  $ opens 0 (Violin 1))
    ||| (delay 3 . stretch 1.2  . setPart (Violin 4)  $ opens 0 (Violin 1))

opens7 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1    . setPart (Cello 2)  $ opens 0 (Cello 1))
    ||| (delay 11 . stretch 1.2  . setPart (Cello 1)  $ opens 0 (Cello 1))

opens8 = setDynamics mf . stretch 1 $ instant
    ||| (delay 0  . stretch 1    . setPart (Cello 2)  $ opens 4 (Cello 1))
    ||| (delay 11 . stretch 1.2  . setPart (Cello 1)  $ opens 5 (Cello 1))

bassHarmMid = stretch 0.82 $ instant
    ||| (delay 0   . stretch 4 . setDynamics mf) dbE'
    ||| (delay 25  . stretch 4 . setDynamics mf) dbA'
    ||| (delay 50  . stretch 4 . setDynamics mf) dbG
    ||| (delay 85  . stretch 4 . setDynamics mf) dbA'
    ||| (delay 120 . stretch 4 . setDynamics mf) dbG
    ||| (delay 150 . stretch 4 . setDynamics mf) dbE''



-- Final score

score :: Score Dur Cue
score = score'
-- score = outro1

score' = stretch 0.8 (harmScore ||| mainScore)

harmScore = instant
    ||| (delay 80    . before 50) harms1
    ||| (delay 143.4 . stretch 4 . setDynamics pp) dbA''
    ||| (delay 150   . reverse . before 50) harms1

    ||| (delay 180 . stretch 4 . setDynamics p) dbA''
    ||| (delay 210 . stretch 4 . setDynamics p) dbA''
    ||| (delay 240 . stretch 4 . setDynamics p) dbG'
    ||| (delay 270 . stretch 4 . setDynamics p) dbA''
    ||| (delay 300 . stretch 4 . setDynamics p) dbG'

    ||| delay 450 bassHarmMid

    ||| (delay 733.9 . stretch 5 . setDynamics pp) dbE

mainScore = instant
    >>> intro1

    >>> rest 10
    >>> present1
    >>> rest 10
    >>> anticipate 30 present2 (anticipate 40 present3 present3high)
    >>> rest 5

    >>> midtro1
    >>> (midCanon ||| midCanonCellos)
    >>> midtro2

    >>> rest 5
    >>> anticipate 40 canon2upper canon2lower
    >>> anticipate 15 (finalCanon ||| finalCanonBass) outro1

\end{code}







Test and utility functions
========

This chapter contains some extra definitions for testing and rendering.

Random number sources
----------

We use a fixed random sequence stored in an extrenal module (which defines the variable `rand`).
To use a different sequence, set rand to another generator, i.e. the standard.

\begin{code}

-- rand' = System.IO.Unsafe.unsafePerformIO System.Random.newStdGen
rand' = rand

randomInts r     = randoms r :: [Int]
randomDoubles r  = randoms r :: [Double]
randomPatterns r = fmap (truncate . (* 3)) $ randomDoubles r

randomInts' r     = randomInts rand'
randomDoubles' r  = randomDoubles rand'
randomPatterns' r = randomPatterns rand'

\end{code}

Open strings and harmonics
----------

These scores contains all available harmonics and open strings respectively (good for hearing intonation etc).

\begin{code}
allHarmonics :: Score Dur Cue
allHarmonics = stretch (1/3) . concatSeq $ do
    part <- List.reverse ensemble
    str <- enumFrom I
    pos <- [0..7]
    return $ setPart part $ naturalHarmonic str pos

allOpenStrings :: Score Dur Cue
allOpenStrings = stretch (1/3) . concatSeq $ do
    part <- List.reverse ensemble
    str <- enumFrom I
    return $ setPart part $ openString str

\end{code}


Miscellaneous
----------

\begin{code}
splitted :: RandomGen r => r -> [r]
splitted = List.unfoldr (Just . System.Random.split)

showPitch :: Pitch -> String
showPitch x = sp p' ++ maybe "" ((" " ++) . show) a ++ " " ++ show (o - 4)
    where
        (p, a) = toDiatonic x
        (o, p') = p `quotRem` 7
        sp 0 = "C"
        sp 1 = "D"
        sp 2 = "E"
        sp 3 = "F"
        sp 4 = "G"
        sp 5 = "A"
        sp 6 = "B"

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (x, (y, z)) = ((x, y), z)

moveMaybe :: [(a, (b, Maybe c))] -> ([(a, b)], [c])
moveMaybe xs = (ys, catMaybes zs)
    where 
        (ys, zs) = unzip $ map assoc xs

\end{code}

As the Cairo backend insists on outputting one file for each page, we add these utility functions to
join the generated "page" files into a single "book" file, as well as removing the intermediate page
files. These depend on Posix functions and the PDF toolkit being installed.

\begin{code}
joinPdfs :: [FilePath] -> FilePath -> IO ()
joinPdfs ins out =
    execute "pdftk" args
    where
        args = ins ++ ["cat", "output", out]

removeFiles :: [FilePath] -> IO ()
removeFiles fs = execute "rm" fs
\end{code}   

Useful to fix the type of a score.

\begin{code}
sc :: Score Dur a -> Score Dur a
sc = id

putSc :: Show a => Score Dur a -> IO ()
putSc = putStr . printScoreEvents . sc
\end{code}


This variable contains number of events in the main score.

\begin{code}
scoreEvents = numberOfEvents . renderTremoloEvents $ score >>= renderCueToMidi
\end{code}


The main function allow us to compile this module into a standalone program (which simply generates the piece when run).

\begin{code}
main = writeMidi "Passager.mid" (render score)
\end{code}



