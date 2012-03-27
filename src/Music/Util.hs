
module Music.Util
(
negateWhen,
negateUnless
)
where

-- | Negate when a boolean condition holds.
negateWhen :: Num a => Bool -> a -> a
negateWhen True  = id
negateWhen False = negate

-- | Negate unless a boolean condition holds.
negateUnless :: Num a => Bool -> a -> a
negateUnless False = id
negateUnless True  = negate
