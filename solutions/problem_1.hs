sumOfMultiples m1 m2 number = 
    (sumOfMultiple m1 number) + (sumOfMultiple m2 number) - (sumOfMultiple (m1*m2) number)


sumOfMultiple multiple number = let
    lastTerm = ((number-1) `div` multiple) * multiple
    in
        arithmeticSeries multiple lastTerm multiple

arithmeticSeries firstTerm lastTerm constant =
    (numberTerms * (firstTerm + lastTerm)) `div` 2
    where 
        numberTerms = ((lastTerm - firstTerm) `div` constant) + 1

