module Data.RunMemo (
    runMemo
  , noMemo
  , Memoizable
  , Memoizer
  ) where

type Memoizable a = a -> a
type Memoizer a b = (a -> b) -> a -> b

runMemo :: Memoizer a b -> Memoizable (a -> b) -> a -> b
runMemo memo f = fix (f . memo)
  where fix h = let x = h x in x
        (g . h) x = g (h x)

noMemo :: Memoizer a b
noMemo f = f
