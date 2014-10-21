cplist = [(3, "Crackle"), (5, "Pop")]

cracklepop num = putStrLn (take_nonempty (cpop num) (show num))

cpop num = cplist >>= (cpop' num)
cpop' num (d, word) = if num `divisible_by` d then word else ""

take_nonempty [] ys = ys
take_nonempty xs ys = xs

divisible_by num x = rem num x == 0

main = mapM cracklepop [1..100]
