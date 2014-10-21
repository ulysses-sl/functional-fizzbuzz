cplist = [(3, "Crackle"), (5, "Pop")]

cracklepop num = putStrLn (take_nonempty (cpop num, show num))

cpop num = cplist >>= \(d, word) -> if num `divisible_by` d then word else ""

take_nonempty (xs,ys) = if null xs then ys else xs

divisible_by num x = rem num x == 0

main = mapM cracklepop [1..100]
