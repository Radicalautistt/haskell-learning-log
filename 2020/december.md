# December
---

---
date: 2020-12-14
---

### Free `Aeson.FromJSON` instances with `deriving-aeson` and `-XDerivingVia`.
```haskell
{-# Language DerivingVia #-}
{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language KindSignatures #-}
{-# Language TypeApplications #-}
{-# Language DerivingStrategies #-}

import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Data.ByteString (ByteString)
import qualified Data.Aeson as Aeson
import qualified Deriving.Aeson as DAeson
import qualified Deriving.Aeson.Stock as DAeson.Stock

-- | Let's suppose that we are writing a wrapper over myanimelist.net API.
-- | It responds with JSON, so we might need to convert it into our custom 
-- | Haskell type. Here is a simplified version of a record to represent Anime.
data Anime = MkAnime {
  animeTitle :: Text
  , animeScore :: Int
  , animeEpisodes :: Int
  , animeOpeningThemes :: [Text]
}
-- | But the JSON we receive doesn't have this 'anime' prefix
-- , and also it's snake cased instead of camel case.
-- | So, the only option we are seemingly left with is to write 
-- | a trivial and boilerplate FromJSON instance. 
-- | It's ok if we need to do this for several types, even if they have 10-20 fields.
-- | But if we want to support the entire myanimelist API it would be very tedious
-- | to write those instances for, say, 30 records with 10-20 fields each.
-- | Thus, deriving-aeson (not to confuse with aeson-deriving (yep, haskell's ecosystem for you)) comes to resque with its CustomJSON newtype.
-- | Our goal is to drop this 'anime' prefix and convert everything to snake case, which is easily doable at this point.
type SnakeCaseWithoutPrefix (prefixToDrop :: Symbol) (record :: Type) = 
  DAeson.CustomJSON '[
    DAeson.FieldLabelModifier (
     -- | Note that order of these modifiers does matter.
     DAeson.StipPrefix prefixToDrop
     , DAeson.CamelToSnake 
     )
  ] record
  
data Anime = MkAnime {
 ...
} deriving stock (Generic, Show)
  deriving Aeson.FromJSON via SnakeCaseWithoutPrefix "anime" Anime
                         -- | This should be replaced by the DAeson.Stock.PrefixedSnake
                         -- , since it is practically the same, and the only reason
                         -- | I've defined this alias here is to show its inner structure.
testAnime :: ByteString
testAnime = " { \
   \ \"title\": \"Serial Experiments Lain\", \
   \ \"score\": 8.02, \
   \ \"episodes\": 13, \
   \ \"opening_themes\": [\"Boa - Duvet\"] \
   \ } "

ghci> Aeson.eitherDecodeStrict' testAnime
ghci> Right (MkAnime {animeTitle = "Serial Experiments Lain", animeScore = 8.02, animeEpisodes = 13, animeOpeningThemes = ["Boa - Duvet"]})
```
