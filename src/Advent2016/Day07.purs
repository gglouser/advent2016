module Advent2016.Day07 where

import Prelude
import Data.Array (length, filter)
import Data.Either (fromRight)
import Data.String (trim)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Partial.Unsafe (unsafePartial)

mkre :: String -> Regex
mkre s = unsafePartial $ fromRight $ regex s noFlags

re :: { supABBA :: Regex, hypABBA :: Regex, supHyp :: Regex, hypSup :: Regex }
re =
    let seq = "(?:\\w|\\[\\w*\\])*"   -- sequence of sup char OR whole hyps
        abba = "(.)(?!\\1)(.)\\2\\1"
        aba = "(.)(?!\\1)(.)\\1"
        bab = "\\2\\1\\2"
    in  { supABBA: mkre $ "^" <> seq <> abba
        , hypABBA: mkre $ "\\[\\w*" <> abba
        , supHyp: mkre $ aba <> seq <> "\\[\\w*" <> bab
        , hypSup: mkre $ aba <> "\\w*\\]" <> seq <> bab
        }

day07 :: String -> { countTLS :: Int, countSSL :: Int }
day07 input =
    let addrs = lines $ trim input
        tlsCheck addr = test re.supABBA addr && not (test re.hypABBA addr)
        sslCheck addr = test re.supHyp addr || test re.hypSup addr
    in  { countTLS: length $ filter tlsCheck addrs
        , countSSL: length $ filter sslCheck addrs
        }
