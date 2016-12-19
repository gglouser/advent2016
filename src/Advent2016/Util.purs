module Advent2016.Util
( mkre
, readInt
, lines
, partitionEithers
, mfilter
, count
, Arg(..)
, bestFirstSearch
) where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadZero (class MonadZero, empty)
import Data.Array (snoc, uncons, insert)
import Data.Either (Either(..), fromRight)
import Data.Foldable (class Foldable, foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Set as S
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

mfilter :: forall m a. MonadZero m => (a -> Boolean) -> m a -> m a
mfilter p m = do
    a <- m
    if p a then pure a else empty

count :: forall f a. Foldable f => (a -> Boolean) -> f a -> Int
count p = foldl (\s v -> if p v then s+1 else s) 0

-----

data Arg a b = Arg a b

instance eqArg :: Eq a => Eq (Arg a b) where
    eq (Arg x _) (Arg y _) = x == y

instance ordArg :: Ord a => Ord (Arg a b) where
    compare (Arg x _) (Arg y _) = compare x y

-----
-- Best-First Search

bestFirstSearch :: forall a. Ord a =>
        (a -> Array a)     -- branch function
     -> (a -> Boolean)     -- goal test
     -> (a -> Int)         -- cost heuristic
     -> a                  -- initial state
     -> Maybe a
bestFirstSearch branch goal cost s0 = tailRec go init
    where
        init = { q:[Arg 0 s0], visited: S.empty }
        go {q, visited} =
            case uncons q of
                Just {head:(Arg _ s), tail} ->
                    if S.member s visited
                    then Loop {q:tail, visited}
                    else if goal s
                        then Done (Just s)
                        else let new = (Arg <$> cost <*> id) <$> branch s
                                 newQ = foldl (flip insert) tail new
                             in Loop {q:newQ, visited: S.insert s visited}
                Nothing -> Done Nothing
