cracklepop :: Int -> [Char]
cracklepop num = cpop num False [(3, "Crackle"), (5, "Pop")]

cpop :: Int -> Bool -> [(Int, [Char])] -> [Char]
cpop num divisible [] = if divisible then [] else show num

cpop num old_div ((x, y):xs) = word ++ cpop num (old_div || new_div) xs
    where
        new_div = rem num x == 0
        word = if new_div then y else []

main :: IO [()]
main = mapM (putStrLn . cracklepop) [1..100]
