
--Задание 1
natural:: Int -> [Int]
natural 0 = []
natural n = natural(n-1) ++ [n]

--Задание 2
nechet:: Int-> [Int]
nechet 0 = []
nechet n = nechet(n-1) ++ [n*2-1]

--Задание 3
chet:: Int -> [Int]
chet 0 = []
chet n = chet(n-1) ++ [n*2]

--Задание 4

kvadrat:: Int -> [Int]
kvadrat 0 = []
kvadrat n = kvadrat(n-1) ++ [n*n]

--Задание 5
fac:: Int -> [Int]
factorial 0 = 1
factorial n = n * factorial (n - 1)
fac 0 = [ ]
fac n = fac (n-1)++[factorial(n)] 


--Раздел 2

--Задача 1

avg xs = sum xs /fromIntegral(length xs) 

--Задача 2

element:: Int -> [Int] -> [Int]
element 1 (x:xs) = xs
element n (x:xs) = x: element (n-1) xs

--Задача 3

list:: [Int] -> [Int] -> [Int]


list n x | length n == length x = zipWith (+) n x
         | otherwise = zipWith (+) a b
    where a = n ++ y
          b = x ++ y 
          y = replicate m 0
          m = abs (length n - length x)

-- Задача 4



