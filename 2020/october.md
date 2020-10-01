# October
___

---
date: 2020-10-01
---

### Japanese kanji/hiragana/katakana could be used to define haskell functions/types. すごいですよ.
```haskell
{-# Language DerivingVia #-}

import Control.Applicative (Alternative, some)

-- | youni - for (in order to) in japanese
ように_ :: Traversable て => Applicative ふ => て あ -> (あ -> ふ べ) -> ふ ()
ように_ = flip traverse_

-- | Good old Parser
newtype N_パルセル レサルト = Mkパルセル { パルス :: String -> Maybe (レサルト, String) }
        deriving (Functor, Applicative, Alternative) via StateT String Maybe

-- | mitasu - fulfill, satisfy; jutsugo - predicate
満たす　:: (Char -> Bool) -> N_パルセル Char
満たす 述語 = Mkパルセル $ \ インプット -> case インプット of
            "" -> Nothing
            あ : レスト　| 述語 あ -> Just (あ, レスト)
                      | otherwise -> Nothing

チャル　:: Char -> N_パルセル　Char
チャル = 満たす . (==)

ghci> パルス (some (チャル 'a')) "aaaa"
ghci> Just ("aaaa", "")

ghci> ように_ "vladimir putin" print
'v'
'l'
'a'
'd'
'i'
'm'
'i'
'r'
' '
'p'
'u'
't'
'i'
'n'
```
___

---
date: 2020-10-02
---

### Example of using `Control.Lens.zoom` with newtyped monad stack.
```haskell 
{-# Language DeriveGeneric #-}
{-# Language BlockArguments #-}
{-# Language OverloadedLabels #-}
{-# Language TypeFamilies #-}
{-# Language TypeApplications #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language UndecidableInstances #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Zoom where

import GHC.Generics (Generic)
import Control.Lens (Zoomed, Zoom (..), zoom, use, each, _1, _2, (.=), (+=), (-=))
import Data.Generics.Labels ()
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT)

newtype GameT gameState monad result = MkGameT { runGameT :: ReaderT Double (StateT gameState monad) result }
        deriving newtype (Functor, Applicative, Monad
                        , MonadIO, MonadReader Double
                        , MonadState gameState)

type instance Zoomed (GameT gameState monad) = Zoomed (ReaderT Double (StateT gameState monad))
instance Monad monad => Zoom (GameT gameState monad) (GameT newGameState monad) gameState newGameState where
         zoom _lens (MkGameT _game) = MkGameT (zoom _lens _game)

data Unit = MkUnit { hp :: Int, position :: (Double, Double) } deriving stock (Show, Generic)
data GameState = MkGameState { units :: [Unit], timer :: Double } deriving stock Generic

type Game result = GameT GameState IO result

initializeGame :: Game ()
initializeGame =  do
  _timer <- ask @Double
  let unit = MkUnit 20 (10, 20)
  #units .= replicate 20 unit
  #timer .= _timer

initialGameState :: GameState
initialGameState =  MkGameState [] 0

-- | We can ommit repeating something like:
-- | { do #units . each -= damage; #units . each . #position .= (0, 0); ... }
-- | And simply zoom in that #units.each part of our big state, since we only modifying that part anyway.
-- | Thus, the aforementioned example becomes:
-- | zoom (#units . each) { do #hp -= damage; #position .= (0, 0); ...}
harassUnits :: Int -> Game ()
harassUnits damage = zoom (#units . each) do
     #hp -= damage

moveUnits :: Double -> Double -> Game ()
moveUnits distanceX distanceY = zoom (#units . each . #position) do
    _1 += distanceX
    _2 += distanceY

decrementTime :: Double -> Game ()
decrementTime deltaTime = #timer -= deltaTime

-- | Best imperative language for you
gameLoop :: Game ()
gameLoop =  do
  _timer <- use #timer
  _units <- use #units
  harassUnits 5
  moveUnits 10 2
  decrementTime 1
  liftIO (print _units)
  -- | delay thread for 1 sec, otherwise the loop will print all of the results immediately.
  liftIO (threadDelay (1000000))
  unless (_timer <= 0) gameLoop

game :: Game ()
game =  initializeGame *> gameLoop

main :: IO ()
main =  evalStateT (runReaderT (runGameT game) 10) initialGameState

ghci> ugly printing of a list of units every second until the given timer is up, no one needs to see this
```
