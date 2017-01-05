module Advent2016.Day24 where

import Prelude
import Advent2016.Util (bestFirstSearch, lines)
import Control.MonadZero (guard)
import Data.Array (concatMap, cons, delete, drop, filter, fromFoldable, length, mapMaybe,
                   snoc, uncons, zipWith, (!!), (..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String (toCharArray)
import Data.Traversable (minimum, sequence, sum)
import Data.Tuple (Tuple(..), fst)

type Grid = Array (Array Boolean)

parseGrid :: String -> Grid
parseGrid = lines >>> map (toCharArray >>> map (_ == '#'))

parsePOI :: String -> Array (Tuple Char (Tuple Int Int))
parsePOI = lines >>> map (toCharArray >>> number) >>> number
       >>> concatMap row >>> filter (isDigit <<< fst)
    where
        row (Tuple i xs) = map (\(Tuple j c) -> Tuple c (Tuple i j)) xs

number :: forall a. Array a -> Array (Tuple Int a)
number xs = zipWith Tuple (0..(length xs - 1)) xs

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isWall :: Grid -> Tuple Int Int -> Boolean
isWall g (Tuple r c) = fromMaybe true $ (g !! r) >>= (_ !! c)

data State = St (Tuple Int Int) Int

instance eqState :: Eq State where
    eq (St p1 _) (St p2 _) = p1 == p2

instance ordState :: Ord State where
    compare (St p1 _) (St p2 _) = compare p1 p2

getMoves :: State -> Int
getMoves (St _ m) = m

shortestDist :: Grid -> Tuple Int Int -> Tuple Int Int -> Maybe Int
shortestDist g start (Tuple gr gc) = getMoves <$> bestFirstSearch branch goal cost (St start 0)
    where
        branch (St (Tuple r c) m) = do
            p <- [Tuple (r+1) c, Tuple (r-1) c, Tuple r (c+1), Tuple r (c-1)]
            guard $ not (isWall g p)
            pure $ St p (m+1)
        goal (St (Tuple r c) _) = r == gr && c == gc
        cost (St (Tuple r c) m) = m + abs (r - gr) + abs (c - gc)

allDists :: Grid -> Array (Tuple Char (Tuple Int Int)) -> Map.Map Char (Map.Map Char Int)
allDists g xs = Map.fromFoldableWith Map.union do
    i <- 0 .. (length xs - 2)
    j <- (i+1) .. (length xs - 1)
    fromMaybe [] do
        Tuple c1 p1 <- xs !! i
        Tuple c2 p2 <- xs !! j
        d <- shortestDist g p1 p2
        pure [Tuple c1 (Map.singleton c2 d), Tuple c2 (Map.singleton c1 d)]

insertions :: forall a. a -> Array a -> Array (Array a)
insertions x xs = (x : xs) : case uncons xs of
    Nothing -> []
    Just {head,tail} -> map (cons head) $ insertions x tail

permutations :: forall a. Array a -> Array (Array a)
permutations xs = case uncons xs of
    Nothing -> [[]]
    Just {head,tail} -> insertions head =<< permutations tail

pathLen :: Map.Map Char (Map.Map Char Int) -> Array Char -> Maybe Int
pathLen dists xs = sum <$> sequence (zipWith dist xs (drop 1 xs))
    where
        dist a b = Map.lookup a dists >>= Map.lookup b

findBest :: Map.Map Char (Map.Map Char Int) -> Maybe Int
findBest dists = minimum $ mapMaybe (pathLen dists) paths
    where
        paths = map (cons '0') <<< permutations <<< delete '0' <<< fromFoldable $ Map.keys dists

findBest2 :: Map.Map Char (Map.Map Char Int) -> Maybe Int
findBest2 dists = minimum $ mapMaybe (pathLen dists) paths
    where
        paths = map (cons '0' >>> flip snoc '0') <<< permutations <<< delete '0' <<< fromFoldable $ Map.keys dists

day24 :: String -> { part1 :: Int, part2 :: Int }
day24 input = { part1: fromMaybe (-1) (findBest dists)
              , part2: fromMaybe (-1) (findBest2 dists)
              }
    where
        grid = parseGrid input
        pois = parsePOI input
        dists = allDists grid pois
