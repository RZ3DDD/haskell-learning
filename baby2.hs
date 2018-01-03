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

-- Функция двойного выполнения передаваемой ей функции
twice :: Num a => (a -> a) -> (a -> a)
twice f x = f (f x)

-- Определение square через тип Num
square :: Num a => a -> a
square x = x * x


-- Четвёртая степень (quad) посредством выполнения функции square дважды
quad :: Num a => a -> a
quad = twice square

-- Четвёртая степень (quad) посредством композиции второй (square)
quad' :: Num a => a -> a 
quad' x = (square.square) x

-- Определение композии попроще
quad'' :: Num a => a -> a
quad'' = (quad.quad) 



-- Разные функции

plusc :: Num a => a -> a -> a
plusc x y = x + y

successor :: Num a => a -> a
successor = plusc 1

myFunc :: String -> String
myFunc s = reverse s

greater :: (Num a, Ord a) => (a, a) -> a
greater (x, y) | x >= y    = x
               | otherwise = y
               
               
sumOfLast2dig :: Int -> Int
sumOfLast2dig x = d0 + d1
              where d0 = x `mod` 10
                    d1 = shift `mod` 10
                    shift = x `div` 10

sumOfLast2dig' :: Int -> Int
sumOfLast2dig' x =
               let d0 = x `mod` 10
                   d1 = shift `mod` 10
                   shift = x `div` 10
               in  d0 + d1


sumOfLast2dig'' :: Int -> Int
sumOfLast2dig'' x =
               let d0 = x `mod` 10
                   d1 = shift `mod` 10
               in  d0 + d1
               where shift = x `div` 10

-- Сие сейчас нельзя, а раньше было можно.
-- Такой паттерн недопустим!
{-
ff :: Int -> Int
ff (x + 3) = x + 1
-}

headDubl :: [a] -> [a]
headDubl (x:xs) = x:x:xs

length' :: [a] -> Integer
length' []      = 0
length' (_:tl) = 1 + length' tl

-- Собственные делители числа m
denominators :: Integer -> [Integer]
denominators m = filter ((==0).(m `mod`)) [2..m-1]
-- или так (однако предыдущий вариант работает почти в два раза быстрее ;-)
denominators' :: Integer -> [Integer]
denominators' m = filter (\ x -> (m `mod` x) == 0) [2..m-1]

-- Составить список простых чисел в интервале [2..n]
primes :: Integer -> [Integer]
primes n = filter (null.denominators) [2..n]



