
{-# LANGUAGE 
--    MonadComprehensions,
    TransformListComp,
    NoMonomorphismRestriction #-}

import Music.Dynamics
import Music.Time
import Music.Time.Score

--type Sc = Score Integer Integer

test
--, test2 
    :: Score Double Double


test = do
    a <- delay   1 $ chord [1, 2, 3, 4, 5]
    b <- stretch 1 $ chord [1, 2, 3, 4, 5]
    stretch 1 $ return (a + b)

testDyn :: Dynamic Double Double
testDyn = liftTemporal test

--test2 = [ x + y | x <- chord [1..10], y <- chord [1], then stretch 1 ]