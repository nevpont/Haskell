module Queue where

class Queue q where
    push :: a -> q a -> q a
    pop :: q a -> q a
    top :: q a -> a
    emp :: q a -> Bool
    create :: a -> q a