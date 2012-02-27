
# Introduction

*Music* is a Haskell library for music representation and manipulation. It is partially 
a development of the ideas outlined in *An Algebraic Theory of Polymorphic Temporal Media* 
by Paul Hudak, which has previously been implemented by libraries such as *Haskore* and 
*Euterpia*. *Music* is entirely separate these libraries, however.

The main goal of *Music* is to generalise standard music notation, allowing the user
to express any kind of music, including Western, non-Western, classical, traditional, popular,
instrumental, vocal and electronic using a common, semantically well-behaved representation.

To achieve this goal, *Music* avoids depending on specific rhythmical patterns, pitch names,
tuning systems, and instructions; instead provides types representing common representations
of these concepts which can be combined into a coherent musical language. More importantly,
it provides a set of polymorphic types and functions which can be used in conjunction
with the musical representation of choice. This way *Music* is able to express a proper superset 
of the music expressible in standard notation.

This flexibility is attained by a somewhat involved use of the Haskell type system, 
the details of which need not be known precisely by users of the library. Tentative 
users are probably better of reading this reference documentation and adapting its 
examples as a starting point, before trying to fully digest the type signatures of 
the reference documentation, however.


<!-- ### Design principles

  * Emphasis on *relative* values and scaling rather than *absolute* values and positioning 
    * Unit values and binary operations are heavily used in this style.
  * Emphasis on *continous values*, postponing sampling to the final rendering phase
    * This is similar to the approach taken in vector graphics, but dissimilar
      to traditional computer music systems.  
  * Emphasis on *time* as the central musical parameter               
    * The library is organized around the *Temporal* type class and its subclasses *Timed* and *Delayed*
    * Other properties are treated *Temporal* transformers
  * Use of standard type classes
    * *Monoids* for composition
    * *Functors* for transforming structures without respect to time
    * *Applicatives* for transforming structures with respect to time
    * *Monads* for transforming and flattening nested structures      -->

## Design principles

### Relative values

TODO Emphasis on *relative* values and scaling rather than *absolute* values and positioning 

TODO Unit values and binary operations are heavily used in this style.

### Continous values

TODO Emphasis on *continous values*, postponing sampling to the final rendering phase

TODO This is similar to the approach taken in vector graphics, but dissimilar
     to traditional computer music systems.  

### Time as the central musical parameter

TODO The library is organized around the *Temporal* type class and its subclasses *Timed* and *Delayed*

TODO Other properties are treated *Temporal* transformers

### Use of standard type classes

* Use of standard type classes
    * *Monoids* for composition
    * *Functors* for transforming structures without respect to time
    * *Applicatives* for transforming structures with respect to time
    * *Monads* for transforming and flattening nested structures


## Terminology

*Music* use standard music terminology where appropriate. The meaning of terms such as 
*pitch*, *dynamics*, *duration*, *intonation*, *phrase* should be straightforward for most
musicians. 

It is worth noting that standard music theory concepts may often be understood in a both
a general and a more specific sense. For example, *pitch* may refer to property of having 
a discernible frequency, or to a set of values such as *A5* and *Eb4*. In *Music* the more
general sense is used unless otherwise noted.





\pagebreak




# Time

In *Music*, the property of *temporality* (being able to be composed in sequence and parallel)
is separated from that of *duration* (having a known extent in time) and *position*
(having a known position in time). 

    class Temporal d where
        instant :: d a
        (|||)   :: d a -> d a -> d a
        (>>>)   :: d a -> d a -> d a
        (<<<)   :: d a -> d a -> d a

    class Temporal d => Loop d where
        loop :: d a -> d a

    class Temporal d => Reverse d where
        reverse :: d a -> d a
    
The main characteristic of durational values is their ability to be prolonged, shortened or scaled.
In standard notation, duration is represented by a combination of divisivion into bars and beats,
represented by vertical barlines and beaming, and note values, represented by note head shape and
number of flags or beams.

    class Time t => Timed t d | d -> t where
        duration :: d a -> t
        stretch  :: t -> d a -> d a

A characteristic of positional values is their ability to be moved forward and backwards in time.
Position is implicit in standard notation, but is often encountered in audio editing software.
Some scorewriting software such as Sibelius of Finale allow the user to view to position of a note
by selecting it.

    class Time t => Delayed t d | d -> t where
        rest   :: t -> d a
        delay  :: t -> d a -> d a


## Implementations 

### Score

### Event list

### Segment







\pagebreak




# Pitch






\pagebreak




# Dynamics






\pagebreak




# Phrasing






\pagebreak




# Space








\pagebreak




# Rendering

## Sound
### MIDI
### OpenSoundControl

## Graphics

## Standard notation
### MusicXML
### Abc Notation








<!-- ## Basic composition

A *temporal structure* is a structure that can be composed in *sequence* and in *parallel*. The
canonical form of such values is a *melody*, which consists of a set of notes
composed in sequence, and a *chord*, which consists of a set of notes composed in parallel.
More elaborate musical structures can easily be constructed by nested compositions of
sequential of parallel structures.

In standard music notation, composition is usually restricted to three levels. The outermost level is the
score, which is a parallel composition of voices. Each voice in turn is a sequential 
composition of chords, and each chord is a parallel
compositions of notes. (Note that to keep things simple we refer to single notes and rests as
chords here, allthough they are typically not thought of as such).

In *Music*, the concept of *temporality* (being able to be arranged sequentially and parallelly)
is separated from that of *durationality* (having a known extent in time) and *positionality*
(having a known position in time). A characteristic of durational values is their ability to be
prolonged, shortened or scaled. A characteristic of positional values is their ability to be moved
forward and backwards in time.

## Instant

# Time and duration

## Relative time and duration

In *Music*, the function `note` returns a score of duration one. This does not imply that all notes must
be of equal duration, as the duration of a score can easily be changed with `stretch`. However, the 
idiomatic way of working with *Music* is to use relative durations, which are only scaled when required. -->


<!-- ## Index

## Temporal values
Values that supports composition in sequence and in parallel, but not necessarily
have a duration.

## Timed and delayed values
Elements that can be timed, stretched and delayed.

## Pitch
Elements that have a frequency.

## Dynamics
Elements that have a velocity. -->

