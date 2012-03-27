{-# LANGUAGE 
    TypeFamilies #-}

module Notable.Core.Diagrams
(
module Diagrams.Prelude,
module Diagrams.TwoD.Text,
(===), 
(=>=), 
(=<=)
)
where

import Data.Colour ( withOpacity )
import Data.Colour.SRGB ( sRGB24read )
import Diagrams.Prelude hiding ( Render, render, (|||), (===) )
import Diagrams.TwoD.Text ( Text )

infixl 6 =<=
infixl 6 =>=
infixl 6 ===

(===), (=>=), (=<=) :: (V a ~ R2, Semigroup a, Juxtaposable a) => a -> a -> a
    
(=>=) = beside unitX
(=<=) = beside (negateV unitX)
(===) = beside (negateV unitY)

