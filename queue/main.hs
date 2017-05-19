import Queue
import SimpleQueue
import FastQueue
import BalancedQueue
import System.CPUTime

test t n = do
    s <- getCPUTime
    let t1 = foldl (\q x -> push x q) t [2..n]
    putStr "count = "
    print (extr t1)
    --print t1
    e <- getCPUTime
    putStr "time = "
    print (fromIntegral (e - s) / 10^12)

extr q = do
    if emp q then 0
    else (1 + (extr (pop q)))

main = do
    putStrLn "SimpleQueue:"
    let t1 :: SimpleQueue Int; t1 = create 1
    test t1 10000
    putStrLn "\nFastQueue:"
    let t2 :: FastQueue Int; t2 = create 1
    test t2 1000000
    putStrLn "\nBalancedQueue:"
    let t3 :: BalancedQueue Int; t3 = create 1
    test t3 1000000