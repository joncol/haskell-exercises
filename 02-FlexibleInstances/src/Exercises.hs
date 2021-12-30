module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- instance PopQuiz [Bool] -- requires `FlexibleInstances`
-- instance PopQuiz [a]
-- instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)] -- requires `FlexibleInstances`
-- instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a) -- requires FlexibleInstances
-- instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a) -- requires FlexibleInstances
-- instance PopQuiz (r -> IO a) -- requires FlexibleInstances
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- instance PopQuiz (a -> b -> c) -- requires FlexibleInstances
-- instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c)) -- requires FlexibleInstances
-- instance PopQuiz ()
-- instance PopQuiz (a, b, c, a) -- requires FlexibleInstances

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a) -- requires FlexibleInstances
-- instance PopQuiz (Pair a)
-- instance PopQuiz (Pair' a) -- requires FlexibleInstances
