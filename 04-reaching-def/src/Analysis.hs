module Analysis where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Utils as Utils
import qualified Context as Ctx

type Map = Map.Map
type Set = Set.Set
type Ctx = Ctx.Ctx

type Value = Ctx.Value
type Result = Ctx.Result

getVal :: Ast.Label -> Result -> Value
getVal lbl values =
  case Map.lookup lbl values of
    Nothing -> undefined
    Just value -> value

initializeIns :: Ast.LabelSet -> Result
initializeIns lbls =
  let initializer = \result key -> Map.insert key Ctx.emptyDefSet result
      emptyMap = Map.empty :: Result
      listOfLabels = Ast.labelSetToList lbls
  in foldl initializer emptyMap listOfLabels

initializeOuts :: Ast.LabelSet -> Result
initializeOuts lbls = initializeIns lbls

kill :: Utils.Block -> Ast.Stmt -> Value
kill blk stmt =
  case blk of
    Utils.BlockStmtAssign _ v _ ->
      let unknown = Ctx.mkDefSet [Ctx.mkDef v Ctx.UnknownLabel]
          blocks = Utils.blockSetToList (Utils.blocksOf stmt)
          handler b =
            case b of
              Utils.BlockStmtAssign lbl v2 _ ->
                if v == v2 then
                  Ctx.mkDefSet [Ctx.mkDef v (Ctx.KnownLabel lbl)]
                else
                  Ctx.emptyDefSet
              _ -> Ctx.emptyDefSet
          killed = [ handler b | b <- blocks ]
      in Ctx.unionDefSets (unknown : killed)
    _ -> Ctx.emptyDefSet

gen :: Utils.Block -> Value
gen blk =
  case blk of
    Utils.BlockStmtAssign lbl v _ ->
      Ctx.mkDefSet [Ctx.mkDef v (Ctx.KnownLabel lbl)]
    _ -> Ctx.emptyDefSet

computeEntry :: Ctx -> Ast.Label -> Value
computeEntry ctx lbl =
  let stmt = Ctx.prog ctx
  in if Utils.initialLabelOf stmt == lbl then
    let vars = Ast.varSetToList (Utils.freeVars stmt)
        defsList = map (\v -> Ctx.mkDef v Ctx.UnknownLabel) vars
    in Ctx.mkDefSet defsList
  else
    let flows = Utils.flowSetToList (Utils.flowOf stmt)
        inflows =
          [ computeExit ctx l1 | Utils.Flow (l1, l2) <- flows, l2 == lbl ]
    in Ctx.unionDefSets inflows

computeExit :: Ctx -> Ast.Label -> Value
computeExit ctx lbl =
  let stmt = Ctx.prog ctx
      blocks = Utils.blocksOf stmt
      blockOf l blks = case blks of
        [] -> undefined
        x : xs ->
          if Utils.labelOfBlock x == l then x
          else blockOf l xs
      blk = blockOf lbl (Utils.blockSetToList blocks)
      entry = getVal lbl (Ctx.ins ctx)
      killed = kill blk stmt
      generated = gen blk
  in Ctx.unionDefSets [Ctx.differenceDefSets entry killed, generated]
