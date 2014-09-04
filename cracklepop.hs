cplist = [(3, "Crackle"), (5, "Pop")]

cracklepop :: Int -> [Char]
cracklepop num = take_nonempty ((flatten . cpop) num) (show num)

cpop :: Int -> [[Char]]
cpop num = map (div_check num) cplist
    where div_check n (d, word) = if n `divisible_by` d then word else []

take_nonempty :: [a] -> [a] -> [a]
take_nonempty xs ys = if null xs then ys else xs

divisible_by :: Int -> Int -> Bool
divisible_by num x = rem num x == 0

flatten :: [[a]] -> [a]
flatten [] = []
flatten xs = foldl (++) [] xs

main :: IO [()]
main = mapM (putStrLn . cracklepop) [1..100]
