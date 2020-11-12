# November
---

---
date: 2020-11-05
---
### Using `Control.Comonad.Cofree` for building custom data structures.
```haskell
import Data.Foldable (for_)
import Control.Comonad.Cofree (Cofree (..), coiter)

-- | en.wikipedia.org/wiki/Rose_tree
data RoseTree value = MkRoseTree value [RoseTree value]

-- | Thats it. If we want to traverse, fold, etc this tree we would obviously need to define/derive those instances.
testRoseTree :: RoseTree Int
testRoseTree = MkRoseTree 1 [MkRoseTree 2 [MkRoseTree 3 [MkRoseTree 4 []]]]

-- | So, why not reuse the existing ones.
type RoseTree' value = Cofree [] value

-- | Looks much cleaner.
testRoseTree' :: RoseTree' Int
testRoseTree' = 1 :< [2 :< [3 :< [4 :< []]]]

ghci> succ <$> testRoseTree'
ghci> 2 :< [3 :< [4 :< [5 :< []]]]

ghci> toList testRoseTree'
ghci> [1, 2, 3, 4]

ghci> for_ testRoseTree' print
ghci> ...

-- | Non-empty list
type NonEmpty = Cofree Maybe

ghci> coiter (pure @Maybe . succ) 1
ghci> 1 :< Just (2 :< Just (3 :< Just (4 :< ...)))

```
---

### in duckduckgo, `!h <query>` command could be used to search anything on hoogle.
---

---
date: 2020-11-06
---
### Generate lenses in ghci.
```haskell
ghci> :set -XTemplateHaskell
ghci> import Control.Lens (makeLenses, (.~), (&))
ghci> data ZhopaKita = MkZhopaKita { _zalupa :: String, _kentavra :: Int } deriving Show
ghci> data X; makeLenses ''ZhopaKita
ghci> zhopaKita = MkZhopaKita "zalupa" 1
ghci> zhopaKita & zalupa .~ "ukraine"
ghci> MkZalupaKentavra {_zalupa = "ukraine", _kentavra = 1}
```
---

---
date: 2020-11-12
---
### Utilizing `Semigroup's` `(->)` [instance](https://github.com/Radicalautistt/haskell-learning-log/blob/master/2020/september.md#functional-arrow-a---b-is-a-semigroup-instance)
```haskell
-- | Move n values to the end of a list
rotate :: Int -> [value] -> [value]
rotate = drop <> take
-- | rotate times = drop times <> take times 
-- | rotate times list = drop times list <> take times list

-- | Entire explanation could be found at: https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell (4th answer)
ghci> rotate 2 [1..10]
ghci> [3, 4, 5, 6, 7, 8, 9, 10, 1, 2]
```


