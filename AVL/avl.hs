module AVL where

data AVL a = Leaf | Node a Int (AVL a) (AVL a)

node :: Ord a => a -> AVL a -> AVL a -> AVL a
node value left right = Node value (max (height left) (height right) + 1) left right

height :: Ord a => AVL a -> Int
height Leaf = 0
height (Node _ h _ _) = h

--rotate right
small_rotate_right :: Ord a => AVL a -> AVL a
small_rotate_right (Node val _ (Node leftVal _ leftLeft leftRight) right) = node leftVal leftLeft (node val leftRight right)

big_rotate_right :: Ord a => AVL a -> AVL a
big_rotate_right (Node val _ left right) = small_rotate_right (node val (small_rotate_left left) right)

rotate_right :: Ord a => AVL a -> AVL a
rotate_right tree@(Node _ _ (Node _ _ leftLeft leftRight) _) = if height leftRight <= height leftLeft then small_rotate_right tree
                                                                                                      else big_rotate_right tree
--rotate left
small_rotate_left :: Ord a => AVL a -> AVL a
small_rotate_left (Node val _ left (Node rightVal _ rightLeft rightRight)) = node rightVal (node val left rightLeft) rightRight

big_rotate_left :: Ord a => AVL a -> AVL a
big_rotate_left (Node val _ left right) = small_rotate_left (node val left (small_rotate_right right))

rotate_left :: Ord a => AVL a -> AVL a
rotate_left tree@(Node _ _ _ (Node _ _ rightLeft rightRight)) = if height rightLeft <= height rightRight then small_rotate_left tree
                                                                                                         else big_rotate_left tree

balance :: Ord a => AVL a -> AVL a
balance Leaf = Leaf
balance tree@(Node _ _ left right) = if height right - height left == 2 then rotate_left tree
                                     else if height right - height left == -2 then rotate_right tree else tree
                                     
push :: Ord a => a -> AVL a -> AVL a
push value Leaf = node value Leaf Leaf
push value tree@(Node val _ left right) = if value < val then balance (node val (push value left) right)
                                          else if value > val then balance (node val left (push value right)) else tree

find_min :: Ord a => AVL a -> a
find_min (Node val _ Leaf _) = val
find_min (Node _ _ left _) = find_min left

pop_min :: Ord a => AVL a -> AVL a
pop_min (Node _ _ Leaf right) = right
pop_min tree@(Node val _ (Node _ _ Leaf Leaf) right) = node val Leaf right
pop_min tree@(Node val _ left right) = balance (node val (pop_min left) right)

merge :: Ord a => AVL a -> AVL a -> AVL a
merge left Leaf = left
merge Leaf right = right
merge left right = balance (node min_value left (pop_min right))
                   where min_value = find_min right
                                
pop :: Ord a => a -> AVL a -> AVL a
pop _ Leaf = Leaf
pop value (Node val _ left right) = if value < val then balance (node val (pop value left) right)
                                    else if value > val then balance (node val left (pop value right)) 
                                    else merge left right

create :: Ord a => [a] -> AVL a
create values = foldl (\t val -> push val t) Leaf values


instance Show a => (Show (AVL a)) where
    show Leaf = "L"
    show (Node val h left right) = "("++(show left)++ ") v"++(show val)++"-h"++(show h)++" ("++(show right)++")"