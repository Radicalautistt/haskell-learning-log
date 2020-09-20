# `September`

----
date: 2020-09-17
----

### How to create global variable
```haskell
{-# Language BlockArguments #-}

import Control.Concurrent (newTVarIO, readTVar)
import Control.Concurrent.STM (atomically)
import System.IO.Unsafe (unsafePerformIO)

{-# NoInline counter #-}
counter :: TVar Int
counter = unsafePerformIO (newTVarIO 0)

incrementCounter :: IO Int
incrementCounter =  atomically do
       modifyTVar counter succ
       result <- readTVar counter
       pure result
       
main :: IO ()
main = do 
  a <- incrementCounter
  print a
  b <- incrementCounter 
  print b 
  c <- incrementCounter
  print c 
  
ghci> main
1
2
3
```

### Javascript-like string interpolation
```haskell
{-# Language QuasiQuotes #-}

-- | https://hackage.haskell.org/package/here
import Data.String.Here (i)

string :: String 
string = "string"

main :: IO ()
main = print @String [i| javascript-like ${string} interpolation|]

ghci> main
ghci> "javascript-like string interpolation"
```

### Eficiently drop `n` elements from the end of a list
```haskell
dropFromEnd :: Int -> [value] -> [value]
dropFromEnd number list = zipWith const list (drop number list)
```

### Functional arrow (a -> b) is a `Semigroup` instance
```haskell
instance Semigroup result => Semigroup (value -> result) where
         firstAction <> secondAction = \ value -> firstAction value <> secondAction value

incrementAll :: [Int] -> [Int]
incrementAll = fmap succ

squareAll :: [Int] -> [Int]
squareAll = fmap (* 2)

ghci> (incrementAll <> squareAll) [1 .. 10]
ghci> [2,3,4,5,6,7,8,9,10,11,2,4,6,8,10,12,14,16,18,20]
```


### Deriving via `undefined` placeholder
```haskell
newtype ZhopaKita result = MkZhopaKita { zhopaKita :: result }
        deriving (Functor, Applicative, Monad) via ZhopaKita
```

### Modifying specific parts of a structure with `Data.Data.Lens.upon`
```haskell
ghci> import Data.Data.Lens

ghci> [1, 2, 3, 4] & upon head .~ 100
ghci> [100, 2, 3, 4]

ghci> [1, 2, 3, 4] & upon (!! 2) .~ 100
ghci> [1, 2, 100, 4]

-- | It doesn't work well with strict record fields
ghci> data X = X { a, b  :: !Int } deriving Show
ghci> X 0 0 & upon b .~ 5
ghci> X { a = 5, b = 0 }
```

### Let in == do let ...
```haskell
zhopa :: String 
zhopa = let kita = "kita"
        in "zhopa" <> kita

-- | ==
zhopa :: String
zhopa = do 
  let kita = "kita"
  "zhopa" <> kita
  
zalupa :: Int 
zalupa = do
  let b = 30
  20 + b

ghci> print zhopa
ghci> "zhopa kita"
```

### Ghc can evaluate expressions from within the command line
```shell
$ ghc -e "traverse print [1..10]"
$ 1
$ ...
$ 10
$ ...
# If there is a need of an import one can write the following
$ ghc -e ":m Data.Foldable" -e "for_ [1..10] print"
$ ...
```

### Duplicate every element of a list `n` times
```haskell
replicateN :: Int -> [value] -> [value]
replicateN times list = list >>= replicate times
```

### Interesting `Control.Monad.join` usecase
```haskell
-- | It works because of the (->) Monad instance
-- | which could be written as follows:
-- | join :: (value -> value -> result) -> value -> result
square :: Int -> Int
square = join (*)

ghci> square 2
ghci> 4

appendItself :: String -> String
appendItself = join (<>)

duplicate :: value -> (value, value)
duplicate = join (,)

ghci> duplicate 2
ghci> (2, 2)
```

----
date: 2020-09-20
----

### Chain multiple set actions with `Control.Lens.&~` and `State` monad
```haskell
{-# Language DeriveGeneric #-}
{-# Language OverloadedLabels #-}

import Control.Lens
import Data.Generics.Labels ()

zhopa :: (Int, String) -> (Int, String)
zhopa input = input &~ do 
      _1 += 20
      _2 <>= " kentavra"
      
ghci> zhopa (10, "zalupa")
ghci> (30, "zalupa kentavra")
  
data Unit = MkUnit {
     name :: !Text
   , position :: !(Double, Double)
   , healthPoints :: !Int
}    deriving Generic

updateUnit :: Unit -> Unit 
updateUnit unit = unit &~ do
          #name <>= " Scanych"
          #position . _1 += 30
          #healthPoints -= 10

ghci> unit = MkUnit "San" (10, 30) 50
ghci> updateUnit unit
ghci> MkUnit { name = "San Scanych", position = (40.0, 30.0), healthPoints = 40 }
```
