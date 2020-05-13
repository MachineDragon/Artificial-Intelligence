--1a
onlyABC :: [Char] -> Bool
letters = ['a','b','c','A','B','C']
onlyABC (x:xs) = (onlyABC xs)  && x `elem` letters
onlyABC [] = True


--1b
--countPerfectSquares :: [Int] -> Int 
--aba n = floor $ sqrt $ (fromIntegral n::Double)
--countPerfectSquares b = length (filter aba b)

--1c
containsMatch :: Eq a => [(a, a)] -> Bool
isEqual (x,y) = x == y
containsMatch a = elem True (map isEqual a)


--2a

isSymmetric Eq a => [(a, a)] -> Bool
swap (a,b) = (b,a)




--3
 perfectNumbers :: Int -> [Int]
 perfect n = [x | x <- [1..n-1], n `mod` x == 0]
 perfectNumbers n = take n [x| x <- [1..], x == sum(perfect(x))]




--4
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n] in (prime :) $ primeFactors $ div n prime

