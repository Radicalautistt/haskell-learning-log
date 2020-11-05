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
