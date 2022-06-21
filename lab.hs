
--1  Функция max3, по трем целым возвращающая наибольшее из них.
max3:: Integer -> Integer -> Integer -> Integer 
max3 a b c  | a == b && b == c = error "Ошибка"
            | a >= b && a >= c = a
            | b >= a && b >= c = b
            | c >= a && c >= b = c
   
--2 Функция min3, по трем целым возвращающая наименьшее из них.
min3:: Integer -> Integer -> Integer -> Integer 
min3 a b c  | a == b && b == c = error "Ошибка"
            | a <= b && a <= c = a
            | b <= a && b <= c = b
            | c <= a && c <= b = c
            
--3  Функция sort2, по двум целым возвращающая пару, в которой наименьшее из них стоит на первом месте, а наибольшее — на втором.
sort2:: Int -> Int -> (Int, Int)
sort2 a b = if a > b then (b, a) else (a, b)

--4 Функция bothTrue :: Bool -> Bool -> Bool, которая возвращает True тогда и только тогда, когда оба ее аргумента будут равны True. Не используйте при определении 
--функции стандартные логический операции (&&, || и т.п.).
bothTrue :: Bool -> Bool -> Bool
bothTrue a b = if [a, b] == [True, True] then True else False

--5 Функция solve2::Double->Double->(Bool,Double), которая по двум числам, представляющим собой коэффициенты линейного уравнения ax + b = 0, возвращает пару, 
-- первый элемент которой равен True, если решение существует и False 
-- в противном случае; при этом второй элемент равен либо
-- значению корня, либо 0.0.

solve2::Double -> Double -> (Bool,Double)
solve2 a b = if a == 0 && b/=0 then (False, 0.0) else (True, -b/a)

--6  Функция isParallel, возвращающая True, если два отрезка, концы которых задаются в аргументах функции,
-- параллельны (или лежат на одной прямой). Например, значение выражения isParallel (1,1) (2,2) (2,0) (4,2) должно быть
-- равно True, поскольку отрезки (1, 1) − (2, 2) и (2, 0) − (4, 2) параллельны.

isParallel:: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isParallel (a,b) (c,d) (x,y) (i,j) =  (c-a)/(i-x) == (d-b)/(j-y) 

--7  Функция isIncluded, аргументами которой служат параметры
-- двух окружностей на плоскости (координаты центров и радиусы);
-- функция возвращает True, если вторая окружность целиком содержится внутри первой.

isIncluded:: (Double, Double) -> Double-> (Double, Double) -> Double -> Bool
isIncluded (a,b) t (c,d) f = (a - c)^2 + (b - d)^2 < (t - f)^2

--8 Функция isRectangular, принимающая в качестве параметров координаты трех точек на плоскости, и возвращающая True,
-- если образуемый ими треугольник — прямоугольный.

isRectangular:: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (a,b) (c,d) (e,f) = (c-a)*(e-a)+(d-b)*(f-b)==0 || (a-c)*(e-c)+(b-d)*(f-d)==0  || (a-e)*(c-e)+(b-f)*(d-f)==0

--9 Функция isTriangle, определяющая, можно ли их отрезков с
-- заданными длинами x, y и z построить треугольник.

isTriangle:: Double -> Double -> Double -> Bool
isTriangle x y z = z< (x+y) && y < (x+z) && x < (y+z)

--10 Функция isSorted, принимающая на вход три числа и возвращающая True, если они упорядочены по возрастанию или по
-- убыванию.

isSorted:: Int -> Int -> Int -> Bool
isSorted x y z = (x > y && y > z ) || ( x < y && y < z) 
