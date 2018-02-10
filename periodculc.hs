
infix 5 ~=
(~=) :: Float -> Float -> Bool
a ~= b = abs (a - b) < h
         where h = 0.5
--
periodculc :: Float -> Float -> Float -> (Float, Float)
periodculc length maxsection minsection = 
           until goodEnough newPeriod (3, length)
              where
                 goodEnough (n, section) = odd (round n)
                                        --   && section >= minsection
                                           && section <= maxsection
                                           && length  ~= section * (n-1)
                 newPeriod :: (Float, Float) -> (Float, Float)
                 newPeriod (ns, sect) = (ns+1, fromIntegral (round ((length/ns) * 10.0)) / 10.0 )
        

                                    
