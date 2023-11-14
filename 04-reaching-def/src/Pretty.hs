module Pretty where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Ast as Ast
import qualified Analysis as Analysis
import qualified Context as Ctx

type Map = Map.Map
type Ctx = Ctx.Ctx


printOne :: Analysis.Result -> [String] -> Ast.Label -> [String]
printOne records strings label =
  case Map.lookup label records of
    Nothing -> undefined
    Just record ->
      let output = (show label) ++ ": " ++ (show record)
      in strings ++ [output]

print :: Ctx -> String
print ctx =
  let rawProg = Ctx.prog ctx
      labels = Ast.labelSetToList (Ctx.labels ctx)
      rawIns = Ctx.ins ctx
      rawOuts = Ctx.outs ctx
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
