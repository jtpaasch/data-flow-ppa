module Pretty where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Ast as Ast
import qualified Analysis as Analysis

type Map = Map.Map


printOne :: Analysis.Result -> [String] -> Ast.Label -> [String]
printOne records strings label =
  case Map.lookup label records of
    Nothing -> undefined
    Just record ->
      let output = (show label) ++ ": " ++ (show record)
      in strings ++ [output]

print :: Analysis.Ctx -> String
print ctx =
  let rawProg = Analysis.prog ctx
      labels = Ast.labelSetToList (Analysis.labels ctx)
      rawIns = Analysis.ins ctx
      rawOuts = Analysis.outs ctx
      progHeader = "-- Program ------------------------------------------"
      prog = progHeader ++ "\n" ++ (show rawProg)
      insHeader = "-- Entry values -------------------------------------"
      insRecords = Prelude.foldl (printOne rawIns) [] labels
      ins = intercalate "\n" (insHeader : insRecords)
      outsHeader = "-- Exit values --------------------------------------"
      outsRecords = Prelude.foldl (printOne rawOuts) [] labels
      outs = intercalate "\n" (outsHeader : outsRecords)
      allOutputs = [prog] ++ [ins] ++ [outs]
  in (intercalate "\n" allOutputs) ++ "\n"
