-- import baby2.hs

-- Первый опыт самописной функции map
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = (f x) : (map' f xs) 
