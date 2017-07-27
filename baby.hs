doubleMe x = x + x

doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x + x

doubleSmallNumber'  x = (if x > 100 then x else x + x) + 1
doubleSmallNumber'' x =  if x > 100 then x else x + x + 1

conanOBrian = "Это я, Конан О'Брайан!"
{- комментарий Haskell -}
