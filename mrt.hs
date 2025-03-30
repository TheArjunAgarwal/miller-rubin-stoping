import System.IO
import Data.Maybe


pl = [2,3,5,7,11,13,17,19,23]

mrt 2 = True
mrt x = odd x && all (\a -> work s x (expMod a d x) ) [2..(x-2)] where
  (s,d) = sd (x-1)

fastMRT 2 = True
fastMRT x 
  | x > 23 =odd x && all (\a -> work s x (expMod a d x) ) pl 
  | x <= 23 = odd x && all (\a -> work s x (expMod a d x) ) [2..(x-2)]
  where
  (s,d) = sd (x-1)

sd n = if odd n then (0,n) else (\(x,y) -> (x+1,y)) (sd (n `div` 2))

-- work s n x
work 0 n x = (x == 1) || (x == n-1)
work s n x
  |(f == 1) && (x /= 1) && (x /= n-1) = False
  |otherwise = work (s-1) n f  where f = (x*x) `mod` n


expMod a 0 m = 1
expMod a d m
  | even d    = expMod ((a * a) `mod` m) (d `div` 2) m
  | otherwise = (a * expMod a (d - 1) m) `mod` m

firstWitness :: Integer -> Maybe Integer
firstWitness x = go x pl where
  go :: Integer -> [Integer] -> Maybe Integer
  go n [] = Nothing
  go n (a:as) = if work s n (expMod a d n) then go n as else Just a where (s,d) = sd (n-1)


cleanData :: [Maybe a] -> [a]
cleanData [] = []
cleanData (x:xs) = case x of
  Nothing -> cleanData xs
  Just t -> t : cleanData xs


substitute :: (Eq a) => [a] -> [(a, a)] -> [a]
substitute xs subs = map (\x -> fromMaybe x (lookup x subs)) xs

frequencyTable :: [Integer] -> Integer -> [(Integer, Int)]
frequencyTable xs n = [(i, count i) | i <- [1..n]]
  where
    count x = length (filter (== x) xs)


main = do
  let n = 2^21
  let clean_data = cleanData $ map firstWitness [25,27..n]
  let sub_data = substitute clean_data (zip pl [1..])
  let table = frequencyTable sub_data 9
  writeFile "dataH.txt" (unlines $ map (\(x,y) -> show x ++ ":" ++ show y) table)

