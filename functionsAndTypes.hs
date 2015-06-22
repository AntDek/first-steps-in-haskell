import Prelude hiding ((||))

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

safetail :: [Int] -> [Int]
safetail xs
    | null xs = []
    | otherwise = tail xs


(||) :: Bool -> Bool -> Bool
False || b = b
_ || _ = True

remove :: Int -> [a] -> [a]
remove n xs = (take n xs) ++ (drop (n + 1) xs)

revert :: [a] -> [a]
revert xs
    | n == 0 = xs
    | otherwise = revert (drop n xs) ++ revert (take n xs)
    where n = length xs `div` 2

main = do
    print (halve [1, 2, 3, 4])
    print (safetail [1, 2])
    print (safetail [])
    print (True || False)
    print (False || False)
    print (remove 1 [1, 2, 3])
    print (revert [1, 2, 3, 4])