-- |

module Util where

safeIdx [] _ = Nothing
safeIdx (x:xs) n
  | n == 0 = return x
  | n < 0 = Nothing
  | otherwise = safeIdx xs (n - 1)

takeNext = take 1 . drop 1
