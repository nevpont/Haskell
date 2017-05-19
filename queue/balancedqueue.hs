module BalancedQueue where

import Queue

data BalancedQueue a = BalancedQueue [a] [a] Int Int

instance Queue BalancedQueue where
    push x (BalancedQueue pushStack popStack len1 len2) = if len1 + 1 <= len2 then BalancedQueue (x:pushStack) popStack (len1 + 1) len2
                                                                              else BalancedQueue [] (popStack ++ reverse (x:pushStack)) 0 (len1 + len2 + 1)
    pop (BalancedQueue pushStack popStack len1 len2) = if len1 <= len2 - 1 then BalancedQueue pushStack (tail popStack) len1 (len2 - 1)
                                                                           else BalancedQueue [] (tail popStack ++ reverse pushStack) 0 (len1 + len2 - 1)
    top (BalancedQueue pushStack popStack len1 len2) = head popStack
    emp (BalancedQueue pushStack popStack len1 len2) = null pushStack && null popStack
    create x = BalancedQueue [] [x] 0 1

instance Show a => Show (BalancedQueue a) where
    show (BalancedQueue pushStack popStack len1 len2) = show (popStack ++ reverse pushStack)