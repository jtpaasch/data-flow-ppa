module Pretty where

import Data.List (intercalate)

showSet :: Show a => [a] -> String
showSet xs =
  let strings = map show xs
  in "{" ++ (intercalate "," strings) ++ "}"
