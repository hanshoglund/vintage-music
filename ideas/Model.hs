



-- laws:
--  instant and seq forms a Monoid
class Sequential t where           
    instant :: t
    '>>     :: t -> t -> t
    '<<     :: t -> t -> t
    
    -- instant and seq forms a Monoid
class Parallel t where
    empty   :: t
    '||     :: t -> t -> t


-- laws:
--  dur instant and (dur . seq) forms a Monoid

-- What is a (the time)?
--  Could be a set of cues, rational numbers/beats, time codecs
class Sequential t => Durational t a where
    duration :: t -> a


-- Do not specify what operation || is w.r.t. duration
-- Possibilities:
--  Cutting (min)
--  Prolonging (max)
--  Stretching (==)           


-- Standard types also instance of Monad, Arrow etc
-- Need transformers for this?


do 
    d     <- 20
    part1 <- notes d [1, 2, 3, 4, 5]
    



