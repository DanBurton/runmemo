-- Inspired by "Dynamic Programming in Haskell is Just Recursion"
-- by Travis Athougies
-- http://travis.athougies.net/posts/2018-05-05-dynamic-programming-is-recursion.html

-- To see the output of this example, run this in the project's root dir:
-- stack runghc examples/lcs.hs

import Data.List (tails)
import Data.RunMemo (runMemo, noMemo, Memoizable)
import qualified Data.MemoCombinators as Memo
import qualified Data.MemoUgly as Ugly

-- n.b. the "Memoizer" type synonym from Data.RunMemo is not helpful
-- for memoizers of multi-arg functions. Just ignore the Memoizer
-- type synonym. Maybe I'll remove it or something.

naiveLCS :: String -> String -> Int
naiveLCS [] _ = 0
naiveLCS _ [] = 0
naiveLCS (x:xs) (y:ys)
  | x == y = 1 + naiveLCS xs ys
  | otherwise = max (naiveLCS (x:xs) ys)
                    (naiveLCS xs (y:ys))

-- Notice how this is exactly the same as naiveLCS, except that
-- the "recur" function is accepted as an argument,
-- and recursion only happens through calling "recur"
memoizableLCS :: Memoizable (String -> String -> Int)
memoizableLCS recur = go where
  go :: String -> String -> Int
  go [] _ = 0
  go _ [] = 0
  go (x:xs) (y:ys)
    | x == y = 1 + recur xs ys
    | otherwise = max (recur (x:xs) ys)
                      (recur xs (y:ys))

-- With our "memoizable" function defined, we can now memoize it using
-- any memoization strategy that we want

-- Memoize with Data.MemoCombinators
trieMemoizedLCS :: String -> String -> Int
trieMemoizedLCS = runMemo stringTableMemoizer memoizableLCS where
  stringMemoizer = Memo.list Memo.char
  stringTableMemoizer = Memo.memo2 stringMemoizer stringMemoizer


-- Memoize with Data.UglyMemo
uglyMemoizedLCS :: String -> String -> Int
uglyMemoizedLCS = runMemo uglyMemo2 memoizableLCS where
  uglyMemo2 = curry . Ugly.memo . uncurry


-- memoizer written by hand
-- This memoizer must be "initialized" by the full strings s1_0 and s2_0
-- precondition: the memoizable function must only recur on
--  "tails" (certain substrings) of s1_0 and s2_0
-- This is true of the memoizableLCS.
-- Exercise to reader: prove it w/ Liquid Haskell
substringTableMemo :: String -> String -> (String -> String -> r) -> String -> String -> r
substringTableMemo s1_0 s2_0 f = go where
  go s1 s2 = table !! row !! col
    -- The substring with length N is at index numRows - N.
    -- e.g. the full string is at index numRows - length s = 0.
    where row = numRows - length s1
          col = numCols - length s2
  numRows = length s1_0
  numCols = length s2_0
  table = tabulate f (tails s1_0) (tails s2_0)

-- Given a list of row values, a list of column values,
-- and a function f to calculate the value at (row, col)
-- create a "table" (list of lists) such that
-- (table !! row !! col) = f (rows !! row) (cols !! col)
tabulate :: (a -> b -> c) -> [a] -> [b] -> [[c]]
tabulate f rows cols = map (\row -> map (\col -> f row col) cols) rows

-- Memoize with the hand-written table memoizer above.
-- Note that said memoizer must be "initialized" by passing it the original args
-- s1 and s2.
-- I do not claim to understand how ghc will handle garbage collection here,
-- but I tend to believe it will work out as one might hope.
substringMemoizedLCS :: String -> String -> Int
substringMemoizedLCS s1 s2 = runMemo (substringTableMemo s1 s2) memoizableLCS s1 s2


noMemoLCS :: String -> String -> Int
noMemoLCS = runMemo noMemo memoizableLCS

-- Note that there is a disadvantage to memoizing functions at the top level
-- in the way that they are memoized.
-- (As is done for trieMemoizedLCS and uglyMemoizedLCS).
-- The runtime will hold on to all values calculated,
-- and will not garbage collect values which are no longer needed.
-- For potentially better garbage collection,
-- use `runMemo` inline at the usage site.
main :: IO ()
main = do
  putStrLn "Notice how fast the memoized versions are"
  putStr "MemoTrie... "
  print (trieMemoizedLCS "nematode knowledge" "empty bottle")
  putStr "UglyMemo... "
  print (uglyMemoizedLCS "nematode knowledge" "empty bottle")
  putStr "Substring Table memo... "
  print (substringMemoizedLCS "nematode knowledge" "empty bottle")
  putStr "naiveLCS, this should take a few seconds... "
  print (naiveLCS "nematode knowledge" "empty bottle")
  putStr "noMemo, this should be equivalent to nativeLCS... "
  print (noMemoLCS "nematode knowledge" "empty bottle")
