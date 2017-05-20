import AVL

main = do
    print (create [1..5])
    
    let t = create [1..1000]
    let t2 = foldl (\t x -> pop x t) t [1, 4 .. 1000]
    
    print (height t2)