double :: Int -> Int
double x = x + x

factorial :: Int -> Int
factorial n = product [1..n]

average :: [Int] -> Int
average ns = sum ns `div` length ns

myLast :: [a] -> a
myLast xs = head (reverse xs)

removeLast :: [a] -> [a]
removeLast xs = take (length xs - 1) xs

main = do
    print (double 10)
    print (factorial 10)
    print (average [2, 5, 10, 7])
    print (myLast [2,3,4,7])
    print (removeLast [2,3,4,7])