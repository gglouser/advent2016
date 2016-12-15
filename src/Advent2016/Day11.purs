module Advent2016.Day11 where

import Prelude
import Advent2016.Util (lines, bestFirstSearch)
import Control.MonadZero ((<|>), guard)
import Data.Array (concat, fromFoldable, nub, zipWith, (..))
import Data.List as L
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (fromRight)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (global)
import Data.Traversable (all, foldl, sequence, sum, traverse)
import Data.Tuple (Tuple(..), fst, lookup)
import Partial.Unsafe (unsafePartial)

choose :: forall a. Int -> List a -> Array (List a)
choose 0 _ = [Nil]
choose n xs = case L.uncons xs of
                Just {head,tail} -> (Cons head <$> choose (n-1) tail) <> choose n tail
                Nothing -> []

-----
-- Represent each microchip-generator pair as a pair of ints
-- indicating which floor each is on.

newtype Pair = Pair { chip :: Int, rtg :: Int, tag :: String }

instance eqPair :: Eq Pair where
    eq (Pair p1) (Pair p2) = p1.chip == p2.chip && p1.rtg == p2.rtg

instance ordPair :: Ord Pair where
    compare (Pair p1) (Pair p2) = compare p1.chip p2.chip <> compare p1.rtg p2.rtg

instance showPair :: Show Pair where
    show (Pair {chip,rtg,tag}) = tag <> "(" <> show chip <> "," <> show rtg <> ")"

-----
newtype State = State { objs :: List Pair, elev :: Int, moves :: Int, prev :: Maybe State }

instance eqState :: Eq State where
    eq (State s1) (State s2) = s1.elev == s2.elev && s1.objs == s2.objs

instance ordState :: Ord State where
    compare (State s1) (State s2) = compare s1.elev s2.elev <> compare s1.objs s2.objs

instance showState :: Show State where
    show (State s) = show s.objs <> "; elev=" <> show s.elev <> "; moves=" <> show s.moves

-----
safetyCheck :: List Pair -> Boolean
safetyCheck objs = L.null $ L.intersect unshielded gens
    where
        unshielded = L.nub $ L.mapMaybe unsh objs
        unsh (Pair {chip,rtg}) = if chip /= rtg then Just chip else Nothing
        gens = L.nub $ map (\(Pair {rtg}) -> rtg) objs

onFloor :: Int -> Pair -> Boolean
onFloor f (Pair {chip,rtg}) = f == chip || f == rtg

moveChip :: Int -> Int -> Pair -> Array Pair
moveChip oldE newE (Pair obj) = do
    guard $ obj.chip == oldE
    pure $ Pair (obj { chip = newE })

moveRTG :: Int -> Int -> Pair -> Array Pair
moveRTG oldE newE (Pair obj) = do
    guard $ obj.rtg == oldE
    pure $ Pair (obj { rtg = newE })

move :: Int -> Int -> Int -> List Pair -> Array (Tuple (List Pair) (List Pair))
move n oldE newE objs = do
    moving <- choose n objs
    moved <- traverse (\o -> moveChip oldE newE o <|> moveRTG oldE newE o) moving
    pure $ Tuple moving moved

moveBoth :: Int -> Int -> List Pair -> Array (Tuple (List Pair) (List Pair))
moveBoth oldE newE objs = do
    moving <- fromFoldable objs
    moved <- moveChip oldE newE =<< moveRTG oldE newE moving
    pure $ Tuple (L.singleton moving) (L.singleton moved)

branchSt :: State -> Array State
branchSt st@(State {objs, elev, moves}) = nub do
    elev' <- [elev+1, elev-1]
    guard $ elev' >= 0 && elev' < 4
    let candidates = L.filter (onFloor elev) $ objs
    Tuple old new <- move 2 elev elev' candidates
                    <|> moveBoth elev elev' candidates
                    <|> move 1 elev elev' candidates
    let objs' = new <> foldl (flip L.delete) objs old
    guard $ safetyCheck objs'
    pure $ State { objs: L.sort objs', elev: elev', moves: moves + 1, prev: Just st }

goalSt :: State -> Boolean
goalSt (State {objs,elev}) = elev == 3 && all (_ == Pair {chip:3,rtg:3,tag:""}) objs

costSt :: State -> Int
costSt (State s) = s.moves + sum ((\x -> 2*(3 - x)) <$> (a.init <> b)) + 3 - s.elev
    where
        a = L.span (_ < s.elev) <<< L.sort
            <<< L.concatMap (\(Pair {chip,rtg}) -> chip L.: rtg L.: L.Nil)
            $ s.objs
        b = L.drop 2 a.rest

-----
-- Parsing

mkre :: String -> Regex
mkre s = unsafePartial $ fromRight $ regex s global

reGens :: Regex
reGens = mkre "\\w+(?= generator)"

reChips :: Regex
reChips = mkre "\\w+(?=-compatible microchip)"

getObjs :: Regex -> String -> Array String
getObjs re floor = fromMaybe [] $ sequence =<< match re floor

isOnFloor :: Array String -> Int -> Array (Tuple String Int)
isOnFloor objs fnum = Tuple <$> objs <@> fnum

allFloors :: Regex -> Array String -> Array (Tuple String Int)
allFloors re lines = concat $ zipWith (isOnFloor <$> getObjs re) lines (0..3)

makePairs :: Array (Tuple String Int) -> Array (Tuple String Int) -> Maybe (Array Pair)
makePairs chips rtgs = traverse (map Pair <<< pair <<< fst) chips
    where
        pair t = { tag:t, chip:_, rtg:_ } <$> lookup t chips <*> lookup t rtgs

parse :: String -> Maybe (List Pair)
parse = lines >>> (makePairs <$> allFloors reChips <*> allFloors reGens)
              >>> map L.fromFoldable

part2Pairs :: List Pair
part2Pairs =
    Pair { chip:0, rtg:0, tag:"elerium" }
    L.: Pair { chip:0, rtg:0, tag:"dilithium" }
    L.: Nil

day11 :: List Pair -> Maybe { moves :: Int }
day11 input = unsafePartial do
    let s0 = State { objs: input, elev: 0, moves: 0, prev: Nothing }
    State r <- bestFirstSearch branchSt goalSt costSt s0
    pure { moves: r.moves }
