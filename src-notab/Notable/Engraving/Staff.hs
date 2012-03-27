{-# LANGUAGE 
    RankNTypes,
    FlexibleContexts #-}

module Notable.Engraving.Staff
where

import Notable.Core
import Notable.Core.Diagrams
import Notable.Core.Glyphs

noteLines :: Engraving
noteLines = 
    moveOriginBy (r2 (0, -space/2)) $ 
        foldr (===) mempty (replicate 5 noteLine) # moveOriginBy (r2 (0, space * (-1.5))) 
        where
            noteLine  =  hrule 15 # lw 0.025 
                           <> 
                         rect 15 space # opacity 0