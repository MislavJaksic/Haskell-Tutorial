module FERLecture1.Internal
  ( dropFirstThree,
    dropLastThree,
    
    succN
  )
  where



dropFirstThree :: [a] -> [a]
dropFirstThree xs = drop 3 xs

dropLastThree :: [a] -> [a]
dropLastThree xs = take (length xs - 3) xs



succN :: (Enum a) => Int -> a -> a
succN 0 x = x
succN n x = succN (n - 1) (succ x)