{- square :: Integer -> Integer
square x = x * x
-}
{-
-- Четвёртая степень (quad) посредством композиции второй (square)
quad :: Integer -> Integer
quad x = square.square x

-- Определение композии попроще
quad_quad = (quad.quad)
-}
three :: Integer -> Integer
three x = 3

infinity :: Integer
infinity = 1 + infinity

f(x, y) = if (x < 20) then x else y

-- Сортировка списка
sort [] = []
sort (x : xs) = sort [ y | y <- xs, y < x ] ++
               [x] ++
               sort [ y | y <- xs, y >= x]


-- Сумма элементов списка
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Факториал
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

fact' :: Integer -> Integer
fact' n | n==0 = 1
        | n >0 = n*fact(n-1)
        | n <0 = error "Отрицательный аргумент для факториала недопустим!"



thirdDim :: Float -> Float -> Float -> Float
thirdDim vlm d1 d2 = (vlm / d1) / d2

-- Определение square через тип Num
square :: Num a => a -> a
square x = x * x

-- Четвёртая степень (quad) посредством композиции второй (square)
quad :: Num a => a -> a 
quad x = (square.square) x

-- Определение композии попроще
quad_quad :: Num a => a -> a
quad_quad = (quad.quad) 

plusc :: Num a => a -> a -> a
plusc x y = x + y

successor :: Num a => a -> a
successor = plusc 1

