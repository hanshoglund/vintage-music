
Introduction
========

This document is a literate Haskell program describing a (yet unnamed) piece for
string orchestra. The file can be run interactively in a Haskell environment. It also
contains playing instructions, which can be converted to a PDF file using [Pandoc](http://johnmacfarlane.net/pandoc/).

\begin{code}

module Music.Projects.MusicaVitae
where

import Temporal.Media
    
\end{code}


Instrumentation and tuning
--------

  * Violin I-IV
  * Viola I-II
  * Cello I-II
  * Double Bass

The orchestra is split into sections:

  * Violin I, III, Viola I and Cello I tunes A4 to 442 Hz (A3 to 217 Hz)
  * Violin II, IV, Viola II and Cello II tunes A4 to 437 Hz (A3 to 222 Hz)
  * Double bass tunes A1 to 55 Hz

The other strings are tuned using the harmonics of the A-string.

All parts may be doubled. If several parts are doubled but not all, strive for a balance
between the main tuning sections (for instance do not double all upper parts).

\begin{code}

data Section = High | Low | Middle 
    deriving (Eq, Show)

data Part 
    = Violin Int
    | Viola Int
    | Cello Int
    | DoubleBass
    deriving (Eq, Show)

partSection (Violin 1)   = High
partSection (Violin 2)   = Low
partSection (Violin 3)   = High
partSection (Violin 4)   = Low
partSection (Viola 1)    = High
partSection (Viola 2)    = Low
partSection (Cello 1)    = High
partSection (Cello 2)    = Low
partSection DoubleBass   = Middle

sectionTuning Low    = 437
sectionTuning Middle = 440
sectionTuning High   = 442

partTuning = sectionTuning . partSection

\end{code}

Playing techniques
--------
\begin{code}

data OpenStringTechnique 
    = NaturalHarmonic
    | NaturalHarmonicGliss
    | HalfHarmonicTrem
    | OpenStringNote
    | Jete
    | Pizz
    | Snap

data QuarterStringTechnique 
    = OpenQuarterTrem
    | QuarterStoppedStringNote

data StoppedStringTechnique 
    = StoppedStringNote
    | StoppedStringPhrase

\end{code}






Intonation
--------

Many playing techiniques in the score calls for open strings. In this case intonation is
determined solely by the tuning.

In some cases, open-string techniques are used with an above first-position stop. This
should make the open string pitch rise about a quarter-tone step (or at least less than
a half-tone step).

Where stopped strings are used, intonation is determined by context:

 * In solo passages, intonation is individual. No attempt should be made to synchronize
   intontation (on long notes et al) for overlapping solo cues.

 * In unison passages, intonation should be synchronized.

\begin{code}

\end{code}

Öppna strängar/Bakgrund
--------

    nat flageolett
    nat flageolett gliss
    trem (halvflageolett)
    öppen sträng

Kvartsstoppade strängar
--------

    trem kvarsttopp/öppen
    gliss + jete
    pizz
    snap


Stoppade strängar/Förgrund
--------

    Melodik (diatonisk, kromatisk?)
        Hur representera förhållandet till omgivningen?

    Fördela mellan grupper   





