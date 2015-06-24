import Prelude hiding (replicate)

sum100 :: Int
sum100 = sum [x ^ 2 | x <- [1..100]]

replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1..n]]

-- Write function similar to [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
f :: [(Int, Int)]
f = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- A triple (x, y, z) of positive integers is pythagorean if x2+y2=z2.
-- Choose the correct implementation for the function pyths :: Int -> [(Int, Int, Int)]
-- that returns the list of all pythagorean triples whose components are at most a given limit.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

-- A positive integer is perfect if it equals the sum of its factors, excluding the number itself.
-- Choose the correct definition of the function perfects :: Int -> [Int]
-- that returns the list of all perfect numbers up to a given limit.
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (init $ factors x)]
    where factors n = [x | x <- [1..n], n `mod` x == 0]

main = do
    print sum100
    print (replicate 3 True)
    print f
    print (pyths 10)
    print (perfects 500)