module FastQueue where

import Queue

data FastQueue a = FastQueue [a] [a]

instance Queue FastQueue where
    push x (FastQueue pushStack popStack) = FastQueue (x:pushStack) popStack
    pop (FastQueue pushStack popStack) = if null popStack then FastQueue [] (tail (reverse pushStack))
                                                          else FastQueue pushStack (tail popStack)
    top (FastQueue pushStack popStack) = if null popStack then head (reverse pushStack)
                                                          else head popStack
    emp (FastQueue pushStack popStack) = null pushStack && null popStack
    create x = FastQueue [x] []

instance Show a => Show (FastQueue a) where
    show (FastQueue pushStack popStack) = show (popStack ++ reverse pushStack)