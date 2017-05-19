module SimpleQueue where

import Queue

data SimpleQueue a = SimpleQueue [a]

instance Queue SimpleQueue where
    push x (SimpleQueue queue) = SimpleQueue (queue++[x])
    pop (SimpleQueue queue) = SimpleQueue (tail queue)
    top (SimpleQueue queue) = head queue
    emp (SimpleQueue queue) = null queue
    create x = SimpleQueue [x]

instance Show a => Show (SimpleQueue a) where
    show (SimpleQueue queue) = show queue