module Advent2016.Day08 where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (ReaderT(..), ask, lift, runReaderT)
import Data.Array (drop, foldM, index, length, replicate, take, updateAt, zipWith, (!!), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Int (fromString)
import Data.String as S
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Data.Traversable (foldMap, foldl, sequence, traverse)
import Partial.Unsafe (unsafePartial)

data ScreenSize = ScreenSize Int Int

data Op = Rect Int Int | RotateRow Int Int | RotateCol Int Int

parse :: String -> Maybe (Array Op)
parse = S.trim >>> lines >>> traverse pline
    where
        pline l = prect l <|> protr l <|> protc l
        pnums f m = f <$> (fromString =<< m !! 1) <*> (fromString =<< m !!2)
        prect l = pnums Rect =<< sequence =<< match rectOp l
        protr l = pnums RotateRow =<< sequence =<< match rotRowOp l
        protc l = pnums RotateCol =<< sequence =<< match rotColOp l
        rectOp = mkre "rect (\\d+)x(\\d+)"
        rotRowOp = mkre "rotate row y=(\\d+) by (\\d+)"
        rotColOp = mkre "rotate column x=(\\d+) by (\\d+)"
        mkre s = unsafePartial $ fromRight $ regex s noFlags

ix :: forall m. Applicative m => Int -> Int -> ReaderT ScreenSize m Int
ix r c = ReaderT \(ScreenSize w _) -> pure (c + r*w)

rectIxs :: forall m. Applicative m => Int -> Int -> ReaderT ScreenSize m (Array Int)
rectIxs w h = sequence $ ix <$> (0..(h-1)) <*> (0..(w-1))

rowIxs :: forall m. Monad m => Int -> ReaderT ScreenSize m (Array Int)
rowIxs r = do (ScreenSize w _) <- ask
              sequence $ ix r <$> (0..(w-1))

colIxs :: forall m. Monad m => Int -> ReaderT ScreenSize m (Array Int)
colIxs c = do (ScreenSize _ h) <- ask
              sequence $ ix <$> (0..(h-1)) <@> c

update :: forall a. Array a -> Array Int -> Array a -> Maybe (Array a)
update arr ixs vs = foldl (>>=) (Just arr) $ zipWith updateAt ixs vs

rotate :: forall a. Int -> Array a -> Array a
rotate n arr = drop (length arr - n) arr <> take (length arr - n) arr

rotOp :: forall a. Array a -> Array Int -> Int -> Maybe (Array a)
rotOp arr ixs b = traverse (index arr) ixs <#> rotate b >>= update arr ixs

exec :: Array Boolean -> Op -> ReaderT ScreenSize Maybe (Array Boolean)
exec arr (Rect w h) = do ixs <- rectIxs w h
                         lift $ update arr ixs (true <$ ixs)
exec arr (RotateRow r b) = do ixs <- rowIxs r
                              lift $ rotOp arr ixs b
exec arr (RotateCol c b) = do ixs <- colIxs c
                              lift $ rotOp arr ixs b

showScreen :: Int -> Array Boolean -> String
showScreen _ [] = ""
showScreen w arr = foldMap (\p -> if p then "#" else ".") (take w arr)
                    <> "\n" <> showScreen w (drop w arr)

count :: forall f a. Foldable f => (a -> Boolean) -> f a -> Int
count p = foldl (\s v -> if p v then s+1 else s) 0

day08 :: Int -> Int -> String -> Maybe { numOn :: Int, finalScreen :: String }
day08 scrW scrH input = do
    ops <- parse input
    let init = replicate (scrW*scrH) false
        final = fromMaybe [] $ runReaderT (foldM exec init ops) (ScreenSize scrW scrH)
    pure { numOn: count id final
         , finalScreen: showScreen scrW final
         }
