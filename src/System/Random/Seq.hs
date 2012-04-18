
module System.Random.Seq 
where

import System.Random

data RandomSeq = RandomSeq Int [Int]

instance Show RandomSeq where show _ = "{randomSeq}"

randomSeq :: [Int] -> RandomSeq
randomSeq xs = RandomSeq 1 xs

instance RandomGen RandomSeq where
    next  (RandomSeq p xs) = (head xs, RandomSeq p (drop p xs))
    split (RandomSeq p xs) = (RandomSeq (p * 2) xs, RandomSeq (p * 2) (drop p xs))

    -- generate:
    --     take 1000 $ randoms (unsafePerformIO newStdGen) :: [Int]    

