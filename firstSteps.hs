double x = x + x

factorial n = product [1..n]

average ns = sum ns `div` length ns

myLast xs = head (reverse xs)

removeLast xs = take (length xs - 1) xs

main = do
    print (double 10)
    print (factorial 10)
    print (average [2, 5, 10, 7])
    print (myLast [2,3,4,7])
    print (removeLast [2,3,4,7])