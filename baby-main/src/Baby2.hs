module Baby2 where


-- Функции
-- Сначала сигнатура (на самом деле она не обязательна, но признак хорошего тона ;-) ...
-- Затем тело функции ...

twice n = n + n

st :: Int -> Int  
st n = 2 ^ n

factorial :: Integer -> Integer
factorial n = product [1..n]


circumference :: Float -> Float
circumference r = 2 * pi * r

-- Сопоставление с образцом -------------------------------
--
lucky :: Int -> String
lucky 7 = "You are luckyman!"
lucky x = "Oops! " ++ show x ++ " is not lucky number. One more time ..."

-- Факториал через сопоставление с образцом (рекурсия) ---------------
--
factorials :: Integer -> Integer
factorials 0 = 1
factorials n = n * factorials (n - 1)

-- Сложение двух векторов на плоскости через сопоставление с образцом ----
--
vectors2Summ :: (Double, Double) -> (Double, Double) -> (Double, Double) 
vectors2Summ (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Функции изъятия элемнтов кортежа из трёх элементов --------------------
--
first3c :: (a, b ,c) -> a
first3c (a, _, _) = a

second3c :: (a, b ,c) -> b
second3c (_, b, _) = b

third3c :: (a, b ,c) -> c
third3c (_, _, c) = c


rock'n'roll :: Int
rock'n'roll = 43


firstLetter :: String -> String
firstLetter [] = "Oops! String is empty..."
firstLetter all@(y:ys) = "First letter in string " ++ "<" ++ all ++ ">" ++ " is '" ++ [y] ++ "'"



