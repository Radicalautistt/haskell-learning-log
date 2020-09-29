# `September`
___

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
___

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
___

### Eficiently drop `n` elements from the end of a list
```haskell
dropFromEnd :: Int -> [value] -> [value]
dropFromEnd number list = zipWith const list (drop number list)
```
___

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
___

### Deriving via `undefined` placeholder
```haskell
newtype ZhopaKita result = MkZhopaKita { zhopaKita :: result }
        deriving (Functor, Applicative, Monad) via ZhopaKita
```
___

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
___

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
___

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
___

### Duplicate every element of a list `n` times
```haskell
replicateN :: Int -> [value] -> [value]
replicateN times list = list >>= replicate times
```
___

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
___

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
___

---
---
date: 2020-09-27
---

### Derive Monad instances via `Control.Monad.Trans.ComposeT` (mostly useless :))
```haskell
{-# Language DerivingVia #-}
{-# Language DeriveFunctor #-}
{-# Language TypeApplications #-}
{-# Language DerivingStrategies #-}

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.State.Strict (StateT (..), evalStateT, MonadState (..))

newtype FileReader filePath result = MkFileReader { runFileReader :: ReaderT filePath (StateT Int (IdentityT IO)) result }
        -- | Spoiler: The very same instances could be derived with newtype strategy
        -- | deriving newtype (Functor, ...)
        -- | So, yeah, it's not the best example of ComposeT usage for deriving
        -- , but I can't come up with a better one at the moment.
        deriving (Functor, Applicative, Monad, MonadReader filePath, MonadState Int, MonadIO)
        -- | Not as neat as Data.Functor.Compose in infix form, but still
        -- | it could come in handy in some situations.
        via ComposeT (ReaderT filePath) (ComposeT (StateT Int) IdentityT) IO
        
fileReader :: FileReader FilePath String 
fileReader =  do
  filePath <- ask @FilePath
  fileContents <- liftIO (readFile filePath)
  put (length (lines fileContents))
  pure fileContents
  
main :: IO ()
main = print =<< runIdentityT (runStateT (runReaderT (runFileReader fileReader) "default.nix") 0)

ghci> main 
ghci> (<default.nix file contents>, line counter)
```
___

---
date: 2020-09-28
---

### Combine several modules into one import
```haskell
module Project.Exports (module Monad) where

import Control.Monad as Monad
import Control.Monad.State.Strict as Monad
import Control.Monad.Reader as Monad
import Control.Monad.IO.Class as Monad
import Control.Monad.Except as Monad

{- 

module Main where

import Project.Exports (StateT (..), MonadReader (..), void)

-}
```
___

---
date: 2020-09-29
---

### Handy unwrapping of asoociated list of newtypes with `Data.Coerce.coerce`
```haskell 
ghci> import Data.Functor.Identity
ghci> foo = [(Identity "abc", 1), (Identity "def", 2), (Identity "ghi", 3)]
-- |  Instead of mapping foo with something like:
ghci> f = map $ \ (Identity key, Identity value) -> (key, value)
-- |  We can simply coerce it into the associated list of values that are contained within the newtypes
ghci> import Data.Coerce
ghci> :set -XTypeApplications
ghci> coerce @_ @[(String, Integer)] foo
-- |  Type inference can figure out the first type application, so we can ommit it
ghci> [("abc", 1), ("def", 2), ("ghi", 3)]

-- | Haskell file.
{-# Language ScopedTypeVariables #-}

import Data.Coerce (Coercible, coerce)

-- | We can generalize this approach thusly: 
unwrapAList :: forall wrapper key value .
               Coercible (wrapper key) key => 
               Coercible (wrapper value) value => [(wrapper key, wrapper value)] -> [(key, value)]
unwrapAList =  coerce @_ @[(key, value)]

------------------

ghci> unwrapAList foo
ghci> [("abc", 1), ("def", 2), ("ghi", 3)]
ghci> newtype ZhopaKita value = ZhopaKita value deriving Show
ghci> bar = [(ZhopaKita "Abc", ZhopaKita 1), (ZhopaKita "def", ZhopaKita 2), (ZhopaKita "ghi", ZhopaKita 3)]
ghci> unwrapAList bar 
ghci> [("Abc", 1), ("def", 2), ("ghi", 3)]
```
___
