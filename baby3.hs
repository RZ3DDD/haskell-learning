-- Этюды со списками
--
concatr :: [[Char]] -> [Char]
concatr [] = []
concatr (x:xs) = x ++ concatr xs
