{-|
    Module      :  Data.Relation
    Copyright   :  Hans Höglund 2005

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Data.Relation
where

data Relation a b 
    = Constant b
    | Identity                  
    | Function (a -> b)
    | Bijection (a -> b) (b -> a)
    
