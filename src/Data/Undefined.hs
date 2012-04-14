{-|
    Module      :  Data.Trivial
    Copyright   :  Hans Höglund 2005
    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

module Data.Undefined
(
Undefined
)
where

import Data.Trivial

-- | A placeholder for undefined types. Can not be initiated. 
--   The trivial value raises an error similar to 'Prelude.undefined'.
data Undefined = Undefined 
    deriving (Show, Eq, Enum)

instance Trivial Undefined where 
    trivial = error "Data.Undefined.undefined"
