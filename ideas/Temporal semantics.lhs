


Basic operations on temporal values.

What is the smallest possible set?

To recreate temporal-media we need

  * Rests
  * Lifting values to notes
  * Parallel and sequential composition
  * Infinate repetition
  * Truncation
  * Stretch (multiplication)

\begin{code}

-- data Score a

rest :: Score a
note :: a -> Score a
|||  :: Score a -> Score a -> Score a
>>>  :: Score a -> Score a -> Score a

reverse :: Score a -> Score a
loop    :: Score a -> Score a
split   :: Score a -> Score b -> (Score b, Score b)
stretch :: Score a -> Score b -> Score b

-- delay can be implemented i.t.o. (>>>) and (|||)
-- or (>>>) can be implemented i.t.o. delay and (|||)

-- Relate to Integers:
-- a + b  =  a >>> b
-- a - b  =  a >>> reverse b
-- a * b  =  a `stretch` b
-- abs    =  \x -> if (x >= 0) x else reverse x . toInteger

-- Scalar projections:
-- a / b  =  fromInteger . (/) . toInteger
-- signum =  fromInteger . signum . toInteger

\end{code}

A good thing about this is that the definition is separate from duration.

Truncate (durational minimum) and stretch (durational multiplication) is 
implemented as score operations (ignoring content) instead.

**Instances**:

  * Eq is performance equality
  * Ord is duration ordering (shortest to longest)
  * Fractional give durations of rational numbers
    
\begin{code}

Score a -> (a -> Score b) -> Score b



-- return a >>= k  ==  k a
-- m >>= return  ==  m
-- m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
-- fmap f xs  ==  xs >>= return . f


\end{code}