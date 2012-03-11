{-|
    Module      :  Music.Time.Overlay
    Copyright   :  Hans Höglund 2012

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleInstances,
    TypeSynonymInstances,
    DeriveFunctor,
    DeriveFoldable #-}

module Music.Time.Overlay
(            
    (<<|),
    (|>>),
    loopOverlay,
    loopOverlayAll,
)
where

import Music.Time
import Music.Time.Score

infixr 8 <<|
infixr 8 |>>

(<<|) :: Time t => Score t a -> Score t a -> Score t a
(|>>) :: Time t => Score t a -> Score t a -> Score t a

x <<| y  =  y |>> x    
x |>> y  =  x ||| delay t y
    where t = duration x / 2    

loopOverlay :: Time t => Score t a -> Score t a
loopOverlay x = x |>> loopOverlay x

loopOverlayAll :: Time t => [Score t a] -> Score t a
loopOverlayAll xs = l xs xs
    where l xs []     = l xs xs
          l xs (y:ys) = y |>> l xs ys