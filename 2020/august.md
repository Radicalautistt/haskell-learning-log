# `August`
___

----
date: 2020-08-19
----

### One can use String/Text/whatever wrapped in a newtype with or without it's constructor in function calls.
```haskell
{-# Language OverloadedStrings #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text 

newtype WText = MkWText Text
        deriving newtype IsString
     -- | It works only if you've derived this instance.
     -- | Actually, all non-String text representations in Haskell
     -- | are in fact instances of this class. Hence, you can use them
     -- | just like String, in case you have -XOverloadedStrings enabled, of course.
     
printWText :: WText -> IO ()
printWText (MkWText text) = Text.putStrLn text

main :: IO ()
main = printWText "zalupa kentavra"
```
___

----
date: 2020-08-20
----

### One can slightly speed up the sequence of TH calls by using `concat <$> sequence` over the list of such calls.
```haskell
{-# Language TemplateHaskell #-}

import Control.Lens (makeLenses, (^.))

data Action
data SpriteSheet

data Unit = MkUnit {
     _hp :: !Int
   , _action :: !Action
   , _sprite :: !SpriteSheet
}

data GameState = MkGameState { 
     _sound :: !Bool
   , _debugMenu :: !Bool
   , _aliveUnits :: ![Unit]
   , _freeCamera :: !Bool
}    deriving Show

concat <$> sequence [
           makeLenses ''Unit
         , makeLenses ''GameState
         ]
         
ghci> MkGameState True False [] False ^. sound
ghci> True
```
___

### One can fold the list (or other `Foldable`) with lenses and some monoid.
```haskell
ghci> [1 .. 5] ^. traverse . coerced @Int @_ @(Sum Int)
ghci> Sum { getSum = 15 }

ghci> [True, False, True, False] ^. folded . _Unwrapping Any
ghci> Any { getAny = True }

ghci> [id, succ, succ, pred] ^. folded . _Unwrapping Endo & flip appEndo 2
ghci> 3

ghci> fromList [("abc", 1), ("def", 2), ("ghi", 3)] ^. folded . _Unwrapping Sum
ghci> Sum { getSum = 6 }
```
___

----
date: 2020-08-28
----

### `Data.Functor.Compose` deriving via trick (only for `Functor` and `Applicative`)
```haskell
-- | If you need more nested functors, just go on and add them, don't forget to add another Compose, though.
newtype Zhopa value = MkZhopa { getZhopa :: Either String (Maybe (Identity value)) }
        -- | This workds only for Functors and Applicatives
        -- , and not Monad, since Monads don't compose. At least in this way.
        deriving (Functor, Applicative)
             via Either String `Compose`
                         Maybe `Compose` Identity

-- | Another example
newtype Zalupa value = MkZalupa { runZalupa :: String -> Either Int value }
        deriving (Functor, Applicative) via (->) String `Compose` Either Int
```
> NOTE: maybe there is a way to derive `Monad` instances via `Control.Monad.Trans.Compose.ComposeT`. I am 99% sure that it is the thing. It is just that I am too lazy to check this at the moment.
> UPD: Yes, there is. I've wrote it [here](https://github.com/Radicalautistt/haskell-learning-log/blob/master/2020/september.md#date-2020-09-27)
___

### Control.Monad.guard 
```haskell
-- | Suppose we need to write safe division function, which prohibits division by zero.
-- | Normally, we would write something like:
regularSafeDiv :: Int -> Int -> Maybe Int
regularSafeDiv m n | n == 0 = Nothing 
                   | otherwise = Just (m `div` n)
                   
-- | But with Control.Monad.guard we can write semantically equivalent computation in a slightly more elegant way.
guardSafeDiv :: Int -> Int -> Maybe Int
guardSafeDiv m n = do
     guard (n /= 0) -- | Guard only approves the computation if the predicate evaluates to True.
     -- | Otherwise, it returns Alternative.empty, or, in this case, Nothing.
     pure (m `div` n)
     
-- | Yet, we could go further and shorten the above defintion even more.
safeDiv :: Int -> Int -> Maybe Int
safeDiv m n = guard (n /= 0) $> m `div` n
                             -- | We don't need `pure` here because `computation $> value` can be understood as:
                             -- | do { computation; pure value }
ghci> safeDiv 4 2
ghci> Just 2

ghci> safeDiv 4 0
ghci> Nothing
```
___

----
date: 2020-08-29
----

### We can ignore data constructor arguments in pattern matching with {}.
```haskell
data Foo = Foo Int String Bool | ...

isFoo :: Foo -> Bool
isFoo Foo {} = True
isFoo _ = False
```
___
