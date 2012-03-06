
module Music.Util.List
where

cycleTimes :: Int -> [a] -> [a]
cycleTimes n xs = take (n * length xs) $ cycle xs

palindrome :: [a] -> [a]
palindrome []        =  []
palindrome [x]       =  [x]
palindrome (x : xs)  =  x : init xs ++ reverse xs
    
palindrome' :: [a] -> [a]
palindrome' xs = xs ++ reverse xs