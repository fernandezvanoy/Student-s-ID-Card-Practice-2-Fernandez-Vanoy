--First part
incomePeriod :: Int -> String 
incomePeriod x 
    | otherwise = "20" ++ take 2 c ++ "-" ++ [last c]
    where
        b = numToString x
        c = firstThree b

numToString :: Int -> String
numToString x = show x

firstThree :: String -> String
firstThree x = take 3 x

--Second part
stringToNum :: String -> Int
stringToNum x = read x :: Int

middleOnes :: String -> String
middleOnes x = reverse(take 2 (reverse (take 5 x)))

academicProgram :: Int -> String 
academicProgram x
    | d > c = "Administratives"
    | d == c = "Engineering"
    | d < c = "Humanities"
    where
        a = numToString x
        b = middleOnes a
        c = stringToNum b
        d = sumDividers c

sumDividers :: Int -> Int
sumDividers x = sum (dividersList x)

dividersList :: Int -> [Int]
dividersList x = searchDividers x (x-1)

searchDividers :: Int -> Int -> [Int]
searchDividers x 1 = [1]
searchDividers x y
    | x `mod` y == 0 = y : searchDividers x (y-1)
    | otherwise = searchDividers x (y-1)


--Third part


lastThree :: String -> String
lastThree x = reverse(take 3 (reverse x))

admissionNumber :: Int -> String
admissionNumber x 
    | c `mod` 2 == 0 = "num" ++ show c ++ " even"
    | c `mod` 2 > 0 = "num" ++ show c ++ " odd"
    where 
    a = numToString x
    b = lastThree a
    c = stringToNum b




main :: IO()
main = do
    a <- readLn :: IO Int
    putStrLn $ (incomePeriod a) ++ " " ++ (academicProgram a) ++ " " ++ (admissionNumber a)
