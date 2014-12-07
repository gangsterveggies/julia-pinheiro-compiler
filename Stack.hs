module Stack where

newtype Stack a = StackImpl [a]

empty :: Stack a
empty = StackImpl []

isEmpty :: Stack a -> Bool
isEmpty (StackImpl s) = null s

push :: a -> Stack a -> Stack a
push x (StackImpl s) = StackImpl (x:s)

top :: Stack a -> a
top (StackImpl s) = head s

pop :: Stack a -> Stack a
pop (StackImpl (s:ss)) = StackImpl ss

changeTop :: a -> Stack a -> Stack a
changeTop _ (StackImpl []) = StackImpl []
changeTop vl (StackImpl (s:ss)) = StackImpl (vl:ss)
