module Advent2016.Day05 where

import Prelude
import Crypto.MD5 (md5)
import Data.Foldable (fold)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List, (..), modifyAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.String (take, length, singleton)
import Data.String.Unsafe (charAt)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

nextGoodHash :: String -> Int -> Tuple String Int
nextGoodHash key = go
    where
        go n = let hash = md5 $ key <> show n
               in if take 5 hash == "00000"
                  then Tuple hash (n+1)
                  else go (n+1)

type State = { p1 :: String
             , p2 :: List (First String)
             }

type Updater a b = String -> String -> a -> Either b a

update :: Updater State { password :: String, password2 :: String }
update c d {p1,p2} = st'
    where
        p1' = if length p1 < 8 then p1 <> c else p1
        p2MB = do i <- fromString c
                  modifyAt i (append <@> First (Just d)) p2
        p2' = fromMaybe p2 p2MB
        st' = case traverse unwrap p2' of
                Just r2 -> Left { password: p1', password2: fold r2 }
                Nothing -> Right { p1:p1', p2:p2' }

search :: forall a b. String -> (String -> String -> a -> Either b a) -> a -> b
search key updater initSt = go initSt 0
    where
        go st n = case nextGoodHash key n of
                    Tuple h n' ->
                        let c = singleton (charAt 5 h)
                            d = singleton (charAt 6 h)
                        in case updater c d st of
                            Left result -> result
                            Right st' -> go st' n'

day05 :: String -> { password :: String, password2 :: String }
day05 input = search input update { p1: "", p2: (First Nothing) <$ (0..7) }
