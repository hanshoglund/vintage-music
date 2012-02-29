
{-# LANGUAGE 
--    MonadComprehensions,
    TransformListComp,
    NoMonomorphismRestriction #-}

import Prelude hiding (reverse)

import Music
import Music.Render.Graphics
import Music.Utilities

test :: Score Double Int
-- test =
--     let { sc = 
--         line [1..3]
--         ||| line [1..2]
--         ||| reverse (line [1..3])
--         ||| rest 3.1415 >>> note 5
--         ||| arpeggio 0.1 [0..5]
--         ||| line [1..10]
--         ||| line [10..16]
--         ||| reverse (line [11,12,13,14] ||| line [22,23])
--         ||| stretch 1.3 (line [5,6,7])
--         ||| note 1
--         ||| stretch 2 (note 2)
--         ||| concatSeq [line [1,2,3], chord [4,5], line [6,7,8]]
--         ||| note 1
--         ||| (delay 1.3 . stretch (2/3) . reverse ) (concatSeq [line [1,2,3], chord [4,5], line [6,7,8]])
--     } in (concatSeq . take 10 . repeat) sc
--    loop phrase ||| (stretch 1.01 $ loop phrase)


test = line [1..8] ||| stretch 4 (chord [1,2,3] >>> chord [4,5,6])
test1 = render test
test2 = stretch 1.1 $ unrender test1
test3 = render test2


openScore score = do
    writePdf "test.pdf" (renderScore score)
    openFile "test.pdf"
    return ()

main = openScore test