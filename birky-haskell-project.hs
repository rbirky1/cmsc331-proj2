{- Rachael Birky -}
{- CMSC 331 Haskell Project -}
{- 05.01.2014 -}

import qualified Data.Char as Char

-- Checks if an element is a member of the given list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
	| x == a	= True
	| otherwise 	= elem' a xs

--Creates a list of an Int number of a  elements
replicateRecursive :: Int -> a -> [a]
replicateRecursive i a
	   | i == 0 = []
	   | otherwise = a:replicateRecursive(i-1) a

--Replicate with a higher order function
replicate' :: Int -> a -> [a]
replicate' i a  =  concat [a:[] | x <- [0..(i-1)]]

--Splits a list in half
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = 
      if l`mod`2==0 then splitAt (l `div` 2) xs
      else splitAt ((l `div` 2)+1) xs
      where l = length xs


--Computes the sum of divsors
sumOfDivisors :: Int -> Int
sumOfDivisors n = sum[x | x<- [1..n`div`2], n`mod`x==0]


--Merges two sorted lists
merge' :: Ord a => [a] -> [a] -> [a]
--if one list is null, return other list (already sorted)
merge' xs [] = xs
merge' [] xs = xs
--given two non null lists
merge' (x:xs) (y:ys)
       {-if first element of first list is less than
       	    the first element of the second list,
       	    place it in front.
	    Recursive call on the rest of the first list and
	    the entire second list-}
  | x<=y = x: merge' xs (y:ys)
  | otherwise = y:merge' (x:xs) ys


--Haskell Mergesort Implementation
msort :: Ord a => [a] -> [a]
--null list
msort [] = []
--singleton list
msort [x] = [x]
msort aList = merge' first second
      where first = msort (fst(halve aList))
      	    second = msort (snd(halve aList))


elemDemo = do
	 let a = 1
	 let aList = [1,2,3]
	 putStrLn(show(elem a aList))

replicateRecursiveDemo = do
	      let i = 3
	      let a = "tsk"
	      putStrLn(show(replicateRecursive i a))

replicateHigherOrderDemo = do
			 let i = 3
			 let a = "tsk"
			 putStrLn(show(replicate' i a))

halveDemo = do
	  let aList = [1,2,3]
	  putStrLn(show(halve aList))

sumOfDivisorsDemo = do
		  putStrLn(show(sumOfDivisors 496))

mergeDemo = do
	  let first = [2,5,6]
	  let second = [1,3,4]
	  putStrLn(show(merge' first second))

msortDemo = do
	  let aList = [5,1,6,4,2,3]
	  putStrLn(show(msort aList))

main = do
     elemDemo
     replicateRecursiveDemo
     replicateHigherOrderDemo
     halveDemo
     sumOfDivisorsDemo
     mergeDemo
     msortDemo