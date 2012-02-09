{-# LANGUAGE RankNTypes #-}

module ExtRecs 
(
    Name(..),
    Age(..),
    Height(..),
    test1,
    test2
)
where




class Name t where 
    name :: t -> String
class Age t where 
    age :: t -> Int
class Height t where 
    height :: t -> Double

data NameT a = NameT { getNameT :: String, runNameT :: a }  
    deriving (Eq, Show)

data AgeT a = AgeT { getAgeT :: Int, runAgeT :: a }
    deriving (Eq, Show)    

data HeightT a = HeightT { getHeightT :: Double, runHeightT :: a }
    deriving (Eq, Show)
    
instance Name (NameT a) where 
    name    = getNameT
instance Age a => Age (NameT a) where
    age     = age . runNameT
instance Height a => Height (NameT a) where
    height  = height . runNameT

instance Age (AgeT a) where 
    age     = getAgeT
instance Height a => Height (AgeT a) where
    height  = height . runAgeT
instance Name a => Name (AgeT a) where
    name    = name . runAgeT

instance Height (HeightT a) where 
    height  = getHeightT    
instance Name a => Name (HeightT a) where
    name    = name . runHeightT
instance Age a => Age (HeightT a) where
    age     = age . runHeightT


test1 = NameT    "Hans" 
      $ HeightT  1.93
      $ AgeT     24 
      $ ()
      
test2 = NameT    "Erik" 
      $ HeightT  1.85
      $ AgeT     27 
      $ ()