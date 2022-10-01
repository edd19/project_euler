greatestPrimeFactor = last .primeFactors

primeFactors n = filter isPrime (factors n)

factors n = filter (\x -> n `mod` x == 0) ([1..(n `div` 2)] ++ [n])

isPrime n
    | n < 2 = False
    | n == 2 = True
    | n `mod` 2 == 0 = False
    | otherwise = (< 2) . length . filter (== 0) $ map (\x -> n `mod` x) [1,3..(sqrtInt n)]


sqrtInt = floor . sqrt . fromIntegral
