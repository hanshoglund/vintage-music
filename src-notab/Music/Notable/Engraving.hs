
{-# LANGUAGE
    FlexibleContexts #-}

-- | This module handles low-level music engraving, conversion from high-level constructs (represented by the
--   'Notation' type) into vector graphics (represented by the 'Engraving' type).
--
--   For more information, see the corresponding submodule:
--
--     * "Music.Notable.Engraving.System"
--
--     * "Music.Notable.Engraving.Staff"
--
--     * "Music.Notable.Engraving.Chord"
--
module Music.Notable.Engraving
(
    module Music.Notable.Engraving.System,
    module Music.Notable.Engraving.Staff,
    module Music.Notable.Engraving.Chord,
)

where

import Music.Notable.Engraving.System
import Music.Notable.Engraving.Staff
import Music.Notable.Engraving.Chord
