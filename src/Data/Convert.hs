{-|
    Module      :  Data.Convert
    Copyright   :  Hans Höglund 2005

    Maintainer  :  hans@hanshoglund.se
    Stability   :  experimental
    Portability :  portable
-}

{-# LANGUAGE 
    MultiParamTypeClasses,
    FlexibleInstances #-}

module Data.Convert
(     
    Render(..),
    Convert(..)
)
where                

class Render a b where
    render :: a -> b
        
class Convert a b where
    convert   :: a -> b
    reconvert :: b -> a

-- instance Convert a b => Render a b where
--     render = convert

-- instance (Render a b, Render b a) => Convert a b where
--     convert   = render
--     reconvert = render