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
