module Pretty where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Equations as Eqs

type Map = Map.Map
type Set = Set.Set

printOne :: Eqs.RDs -> [String] -> Ast.Label -> [String]
printOne records strings label =
  case Map.lookup label records of
    Nothing -> undefined
    Just record ->
      let output = (show label) ++ ": " ++ (show record)
      in strings ++ [output]

print :: Eqs.Ctx -> String
print ctx =
  let rawProg = Eqs.prog ctx
      progHeader = "-- Program ------------------------------"
      prog = progHeader ++ "\n" ++ (show rawProg)
      labels = Eqs.labels ctx
      rawIns = Eqs.ins ctx
      insHeader = "-- RD entry -----------------------------"
      insRecords = Prelude.foldl (printOne rawIns) [] labels
      ins = intercalate "\n" (insHeader : insRecords)
      rawOuts = Eqs.outs ctx
      outsHeader = "-- RD exit -----------------------------"
      outsRecords = Prelude.foldl (printOne rawOuts) [] labels
      outs = intercalate "\n" (outsHeader : outsRecords)
      allOutputs = [prog] ++ [ins] ++ [outs]
  in (intercalate "\n" allOutputs) ++ "\n"
