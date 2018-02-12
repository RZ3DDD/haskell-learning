-- Begin of periodculc etude

-- (~=) - Операция определения достаточного допуска вычислений (tolerance)
tolerance = 0.5
infix 5 ~=
(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < tolerance

-- Функция periodculc вычисляет количество точек (включая крайние) на заданом отрезке (размером lgth)
-- расположенными через равные промежутки (размером section).
-- Кроме lgth Необходимо задать maxsection и minsection, а также количество итераций (maxiter).
-- Вычисляет только нечетное количество точек, чтобы одна из точек была точно посредине отрезка. 
-- Результат представляется в виде списка кортежей [(количество точек, размер промежутка)]. 
periodculc :: Double -> Double -> Double -> Int -> [(Int, Double)]
periodculc lgth maxsection minsection maxiter = 
     filter pred (map (\ i -> (i, (fromIntegral (round (lgth/(fromIntegral (i-1)) * 10.0))) / 10.0 )) [3..maxiter])
         where
            pred :: (Int, Double) -> Bool
            pred (n, section) = odd n
                             && section <= maxsection
                             && section >= minsection
                             && lgth ~= section * fromIntegral (n-1)      
                             

-- А в periodculc' количество итераций (maxiter) указывать не надо.
periodculc' :: Double -> Double -> Double -> [(Int, Double)]
periodculc' lgth maxsection minsection = 
     filter pred (reverse initialList)
         where
            pred :: (Int, Double) -> Bool
            pred (n, section) = odd n
                             && section <= maxsection
                             && lgth ~= section * fromIntegral (n-1)      
                             
            initialList :: [(Int, Double)]
            initialList =
                until prd newTuple [(3, lgth / fromIntegral 2)]
                   where 
                      prd :: [(Int, Double)] -> Bool
                      prd xs = snd (head xs) < minsection
         
                      newTuple :: [(Int, Double)] -> [(Int, Double)]
                      newTuple xs = ( fst (head xs) + 1,
                                      fromIntegral (round (lgth / fromIntegral (fst (head xs)) * 10.0)) / 10.0
                                    ) : xs  


-- End of periodculc etude
