
--1
max3:: Integer -> Integer -> Integer -> Integer 
max3 a b c  | a == b && b == c = error "Ошибка"
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | c >= a && c >= b = c
   
--2
min3:: Integer -> Integer -> Integer -> Integer 
min3 a b c  | a == b && b == c = error "Ошибка"
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | c <= a && c <= b = c
            
--3
sort2:: Int -> Int -> (Int, Int)
sort2 a b = if a > b then (b, a) else (a, b)

--4
bothTrue :: Bool -> Bool -> Bool
bothTrue a b = if [a, b] == [True, True] then True else False
--5
solve2::Double -> Double -> (Bool,Double)
solve2 a b = if a == 0 && b/=0 then (False, 0.0) else (True, -b/a)

--6

isParallel:: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (a,b) (c,d) (x,y) (i,j) =  (c-a)/(i-x) == (d-b)/(j-y) 

--7 

isIncluded:: (Double, Double) -> Double-> (Double, Double) -> Double -> Bool
isIncluded (a,b) t (c,d) f = (a - c)^2 + (b - d)^2 < (t - f)^2

--8
isRectangular:: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (a,b) (c,d) (e,f) = (c-a)*(e-a)+(d-b)*(f-b)==0 || (a-c)*(e-c)+(b-d)*(f-d)==0  || (a-e)*(c-e)+(b-f)*(d-f)==0

--9

isTriangle:: Double -> Double -> Double -> Bool
isTriangle x y z = z< (x+y) && y < (x+z) && x < (y+z)

--10
isSorted:: Int -> Int -> Int -> Bool
isSorted x y z = (x > y && y > z ) || ( x < y && y < z) 
