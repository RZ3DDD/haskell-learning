-- Begin of periodculc etude

-- (~=) - Операция определения достаточного допуска вычислений (tolerance)
tolerance = 0.5
infix 5 ~=
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < tolerance

-- Функция periodculc вычисляет количество точек (включая крайние) на заданом отрезке (размером lgth),
-- расположенных через равные промежутки (размером section).
-- Кроме lgth Необходимо задать maxsection и minsection.
-- Вычисляет только нечетное количество точек, чтобы одна из точек была точно посредине отрезка. 
-- Результат представляется в виде списка кортежей [(количество точек, размер промежутка)]. 

-- Сначала такое нативное определение ...
periodculc' :: Double -> Double -> Double -> [(Int, Double)]
periodculc' lgth maxsection minsection = 
     filter pred ( map (\ i -> (i, (fromIntegral (round (lgth/(fromIntegral (i-1)) * 10.0))) / 10.0 ))
                       [3.. round (lgth / minsection) + 1]
                 )
         where
            pred :: (Int, Double) -> Bool
            pred (n, section) = odd n
                             && section <= maxsection
                             && lgth ~= section * fromIntegral (n-1)      
                             

-- И самое изящное (скажем так - математическое, посредством абстракции списка) определение сей функции ...
periodculc :: Double -> Double -> Double -> [(Int, Double)]
periodculc lgth maxsection minsection = 
       [(n, section) | n <- [3 .. round (lgth / minsection) + 1] , 
                                   let section = fromIntegral (round (lgth / fromIntegral (n-1) * 10.0)) / 10.0,
                                   odd n,
                                   section <= maxsection,
                                   lgth ~= section * fromIntegral (n-1)
       ]

-- End of periodculc etude
