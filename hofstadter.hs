import Data.List

-- The Hofstadter Figure-Figure sequences
hofR :: [Integer]
hofR = 1 : zipWith (+) hofR hofS

hofS :: [Integer]
hofS = 2 : 4: (complementFromN hofR 5)

-- A helper function that constructs a strictly increasing sequence of
-- numbers (starting from n) that are not in the given strictly increasing sequence
complementFromN :: (Num a, Enum a, Ord a) => [a] -> a -> [a]
complementFromN [] n = [n..]
complementFromN xs n
    | head xs < n  = complementFromN (tail xs) n
    | head xs == n = complementFromN (tail xs) (n+1)
    | otherwise = n : complementFromN xs (n+1)

-- The Hofstadter G sequence:
hofG :: [Integer]
hofG = 0 : zipWith (-) [1..] (map (\x -> genericIndex hofG x) hofG)

-- The Hofstadter H sequence:
hofH :: [Integer]
hofH = 0 : zipWith (-) [1..] (map applyH (map applyH hofH))
    where applyH x = genericIndex hofH x

-- The Hofstadter Male and Female sequences:
hofM :: [Integer]
hofM = 0 : zipWith (-) [1..] (map (\x -> genericIndex hofF x) hofM)

hofF :: [Integer]
hofF = 1 : zipWith (-) [1..] (map (\x -> genericIndex hofM x) hofF)

-- The Hofstadter Q sequence:
hofQ :: [Integer]
hofQ = 1 : 1 : zipWith (+) (map applyQ (zipWith (-) [2..] (tail hofQ)))
                           (map applyQ (zipWith (-) [2..] hofQ))
    where applyQ = \x -> genericIndex hofQ x
