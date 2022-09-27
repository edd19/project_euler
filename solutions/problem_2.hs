
sumOfEvenFibonacciValues n = sum . filter even $ takeWhile (<n) (fibonacci) 

fibonacci = 
    1 : 2 : 3 : fib 2 3
    where
        fib res1 res2 = (res1 + res2) : fib res2 (res1 + res2)
