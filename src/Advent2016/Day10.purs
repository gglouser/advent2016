module Advent2016.Day10 where

import Prelude
import Advent2016.Util (readInt, partitionEithers)
import Control.Alt ((<|>))
import Control.Monad.RWS (RWS, asks, evalRWS, gets, modify, tell)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Array (catMaybes, some, uncons)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.String (fromCharArray)
import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser.Combinators (sepEndBy)
import Text.Parsing.StringParser.String (anyDigit, string, whiteSpace)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), lookup, snd)

type Feed = { bot :: Int, val :: Int }
data Dest = Output Int | Bot Int
type LoHi = { lo :: Dest, hi :: Dest }
type PassMap = M.Map Int LoHi
type HoldMap = M.Map Int Int

feed :: Int -> Int -> Feed
feed b v = { bot: b, val: v }

lohi :: Dest -> Dest -> LoHi
lohi lo hi = { lo: lo, hi: hi }

parse :: String -> Maybe (List (Either Feed (Tuple Int LoHi)))
parse = either (const Nothing) Just <<< runParser (line `sepEndBy` whiteSpace)
    where
        line = Left <$> feedP <|> Right <$> connP
        feedP = string "value " $> flip feed <*> intP <* string " goes to bot " <*> intP
        destP = (string "output " $> Output <|> string "bot " $> Bot) <*> intP
        connP = string "bot " $> Tuple <*> intP
                    <*> (lohi <$ string " gives low to " <*> destP
                              <* string " and high to " <*> destP)
        intP = readInt <<< fromCharArray <$> some anyDigit

data Event = Compare Int Int Int | ToOutput Int Int
type BotNet a = RWS PassMap (Array Event) HoldMap a

send :: Int -> Dest -> BotNet (Maybe Feed)
send v (Bot r) = pure $ Just (feed r v)
send v (Output o) = do
    tell [ToOutput o v]
    pure Nothing

passOn :: Int -> Int -> Int -> BotNet (Array Feed)
passOn bot x y = do
    tell [Compare bot x y]
    conn <- asks (M.lookup bot)
    case conn of
        Just {lo,hi} -> catMaybes <$> sequence [send x lo, send y hi]
        Nothing -> pure [] -- error: bot not found

step :: Feed -> BotNet (Array Feed)
step {bot,val} = do
    holding <- gets (M.lookup bot)
    case holding of
        Just u -> do modify (M.delete bot)
                     if u < val
                       then passOn bot u val
                       else passOn bot val u
        Nothing -> do modify (M.insert bot val)
                      pure []

run :: PassMap -> Array Feed -> Array Event
run passMap initFeeds = snd $ evalRWS (tailRecM run' initFeeds) passMap M.empty
    where
        run' fs = case uncons fs of
                    Nothing -> pure $ Done unit
                    Just {head, tail} -> do
                        fs' <- step head
                        pure $ Loop (tail <> fs')

day10 :: Int -> Int -> String -> Maybe { part1 :: Int, part2 :: Int }
day10 i j input = do
    instrs <- partitionEithers <$> parse input
    let passMap = M.fromFoldable instrs.rights
        initFeeds = instrs.lefts
        result = run passMap initFeeds
        mkLookup (Compare b v1 v2) = Left $ Tuple (Tuple v1 v2) b
        mkLookup (ToOutput o v)    = Right $ Tuple o v
        eventLookup = partitionEithers $ map mkLookup result
    goal <- lookup (Tuple i j) eventLookup.lefts
    o0 <- lookup 0 eventLookup.rights
    o1 <- lookup 1 eventLookup.rights
    o2 <- lookup 2 eventLookup.rights
    pure { part1: goal
         , part2: o0 * o1 * o2
         }
