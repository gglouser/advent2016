module Advent2016.Util
( mkre
, readInt
, lines
, partitionEithers
) where

import Prelude
import Data.Array (snoc)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, foldl)
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Data.String (trim)
import Data.String.Utils as U
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

mkre :: String -> Regex
mkre s = unsafePartial $ fromRight $ regex s noFlags

readInt :: String -> Int
readInt s = unsafePartial $ fromJust $ fromString s

lines :: String -> Array String
lines = trim >>> U.lines

partitionEithers :: forall f a b. Foldable f => f (Either a b) -> { lefts :: Array a, rights :: Array b }
partitionEithers = foldl part { lefts: [], rights: [] }
    where
        part r@{lefts}  (Left a)  = r { lefts = snoc lefts a }
        part r@{rights} (Right a) = r { rights = snoc rights a }
