import Data.List

hofG :: [Integer]
hofG = 0 : zipWith (-) [1..] (map (\x -> genericIndex hofG x) hofG)
