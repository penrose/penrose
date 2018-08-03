-- This module, imported from here https://hackage.haskell.org/package/text-replace
-- preform a textual replacements over a string using a list of rules.
-- In use ar Sugarer.hs for statement notations replacements
module Replace
  (
  -- * Performing replacement
    replaceWithList, replaceWithMap, replaceWithTrie

  -- * Specifying replacements
  , Replace (..), ReplaceMap, listToMap, mapToAscList

  -- * Replacements in trie structure
  , Trie, Trie' (..), listToTrie, ascListToTrie, mapToTrie, drawTrie

  -- * Non-empty string
  , String' (..), string'fromString, string'head, string'tail

  ) where

-- base
import           Control.Arrow      ((>>>))
import qualified Data.Foldable      as Foldable
import           Data.Function      (on)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.String        (IsString (..))

-- containers
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Tree       (Tree)
import qualified Data.Tree       as Tree

{- | Apply a list of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

Internally, the list will be converted to a 'ReplaceMap' using 'listToMap'. If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored.

If you are going to be applying the same list of rules to multiple input strings, you should first convert the list to a 'Trie' using 'listToTrie' and then use 'replaceWithTrie' instead. -}
replaceWithList
  :: Foldable f
  => f Replace -- ^ List of replacement rules
  -> String    -- ^ Input string
  -> String    -- ^ Result after performing replacements on the input string
replaceWithList = listToTrie >>> replaceWithTrie

{- | Apply a map of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

If you are going to be applying the same list of rules to multiple input strings, you should first convert the 'Map' to a 'Trie' using 'mapToTrie' and then use 'replaceWithTrie' instead. -}
replaceWithMap
  :: ReplaceMap -- ^ Map of replacement rules
  -> String     -- ^ Input string
  -> String     -- ^ Result after performing replacements on the input string
replaceWithMap = mapToTrie >>> replaceWithTrie

{- | Apply a trie of replacement rules to a string. The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

To construct a 'Trie', you may use 'listToTrie' or 'mapToTrie'. -}
replaceWithTrie
  :: Trie   -- ^ Map of replacement rules, represented as a trie
  -> String -- ^ Input string
  -> String -- ^ Result after performing replacements on the input string
replaceWithTrie trie = go
  where
    go [] = []
    go xs@(x : xs') =
      case replaceWithTrie1 trie xs of
        Nothing -> x : go xs'
        Just (r, xs'') -> r ++ go xs''

replaceWithTrie1 :: Trie -> String -> Maybe (String, String)
replaceWithTrie1 _ [] = Nothing
replaceWithTrie1 trie (x : xs) =
  case Map.lookup x trie of
    Nothing                  -> Nothing
    Just (Trie' Nothing bs)  -> replaceWithTrie1 bs xs
    Just (Trie' (Just r) bs) -> case replaceWithTrie1 bs xs of
                                  Nothing -> Just (r, xs)
                                  longerMatch -> longerMatch

-- | Non-empty string.
newtype String' = String' (NonEmpty Char)
  deriving (Eq, Ord)

instance Show String'
  where
    showsPrec i (String' x) = showsPrec i (NonEmpty.toList x)

{- | @'fromString' = 'string'fromString'@

Warning: @('fromString' "" :: 'String'') = ⊥@ -}
instance IsString String'
  where
    fromString = string'fromString

{- | Convert an ordinary 'String' to a non-empty 'String''.

Warning: @string'fromString "" = ⊥@ -}
string'fromString :: String -> String'
string'fromString = NonEmpty.fromList >>> String'

{- | The first character of a non-empty string. -}
string'head :: String' -> Char
string'head (String' x) = NonEmpty.head x

{- | All characters of a non-empty string except the first. -}
string'tail :: String' -> String
string'tail (String' x) = NonEmpty.tail x

{- | A replacement rule.

> Replace "abc" "xyz"

means

/When you encounter the string __@abc@__ in the input text, replace it with __@xyz@__./

The first argument must be a non-empty string, because there is no sensible way to interpret "replace all occurrences of the empty string." -}
data Replace =
  Replace
    { replaceFrom :: String' -- ^ A string we're looking for
    , replaceTo   :: String  -- ^ A string we're replacing it with
    }
    deriving (Eq, Show)

{- | A map where the keys are strings we're looking for, and the values are strings with which we're replacing a key that we find.

You may use 'listToMap' to construct a 'ReplaceMap' from a list of replacement rules, and you may use 'mapToAscList' to convert back to a list. -}
type ReplaceMap = Map String' String

{- | Construct a 'ReplaceMap' from a list of replacement rules.

If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored. -}
listToMap :: Foldable f => f Replace -> ReplaceMap
listToMap = Foldable.toList >>> fmap toTuple >>> Map.fromList
  where
    toTuple x = (replaceFrom x, replaceTo x)

{- | Convert a replacement map to a list of replacement rules. The rules in the list will be sorted according to their 'replaceFrom' field in ascending order. -}
mapToAscList :: ReplaceMap -> [Replace]
mapToAscList = Map.toAscList >>> fmap (\(x, y) -> Replace x y)

{- | A representation of a 'ReplaceMap' designed for efficient lookups when we perform the replacements in 'replaceWithTrie'.

You may construct a 'Trie' using 'listToTrie' or 'mapToTrie'. -}
type Trie = Map Char Trie'

{- | A variant of 'Trie' which may contain a value at the root of the tree. -}
data Trie' =
  Trie'
    { trieRoot     :: Maybe String
    , trieBranches :: Trie
    }
  deriving (Eq, Show)

{- | Draws a text diagram of a trie; useful for debugging. -}
drawTrie :: Trie -> String
drawTrie = trieForest >>> Tree.drawForest

trieForest :: Trie -> Tree.Forest String
trieForest =
  Map.toAscList >>>
  fmap (\(c, t) -> trieTree [c] t)

trieTree :: String -> Trie' -> Tree String
trieTree c (Trie' r bs) =
  case (r, Map.toAscList bs) of
    (Nothing, [(c', t)]) -> trieTree (c ++ [c']) t
    _ -> Tree.Node (c ++ maybe "" (\rr -> " - " ++ show rr) r)
                   (trieForest bs)

{- | Convert a replacement map to a trie, which is used to efficiently implement 'replaceWithTrie'. -}
mapToTrie :: ReplaceMap -> Trie
mapToTrie = mapToAscList >>> ascListToTrie

{- | Convert a list of replacement rules to a trie, which is used to efficiently implement 'replaceWithTrie'.

If the list contains more than one replacement for the same search string, the last mapping is used, and earlier mappings are ignored. -}
listToTrie :: Foldable f => f Replace -> Trie
listToTrie = listToMap >>> mapToTrie

{- | Convert a list of replacement rules to a 'Trie', where the rules must be sorted in ascending order by the 'replaceFrom' field.

 Warning: this precondition is not checked. If you are not sure, it is safer to use 'listToTrie' instead. -}
ascListToTrie
  :: Foldable f
  => f Replace  -- ^ This list must be sorted according to the 'replaceFrom'
                --   field in ascending order
                --
                --  Warning: this precondition is not checked
  -> Trie
ascListToTrie =
  NonEmpty.groupBy ((==) `on` (replaceFrom >>> string'head)) >>>
  fmap (\xs -> (firstChar xs, subtrie xs)) >>>
  Map.fromAscList
  where
    firstChar = NonEmpty.head >>> replaceFrom >>> string'head
    subtrie = fmap (\(Replace x y) -> (string'tail x, y)) >>> ascListToTrie'

ascListToTrie'
  :: Foldable f
  => f (String, String)  -- ^ This list must be sorted according to the left
                         --   field of the tuple in ascending order
                         --
                         --  Warning: this precondition is not checked
  -> Trie'
ascListToTrie' = Foldable.toList >>> f
  where
    f :: [(String, String)] -> Trie'
    f (([], x) : xs) = Trie' (Just x) (g xs)
    f xs             = Trie' Nothing (g xs)

    g :: (Foldable f, Functor f) => f (String, String) -> Trie
    g = fmap (\(x, y) -> Replace (string'fromString x) y) >>> ascListToTrie
