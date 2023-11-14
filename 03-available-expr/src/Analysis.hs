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
  let initializer = \result key -> Map.insert key Utils.emptyAexpSet result
      emptyMap = Map.empty :: Result
      listOfLabels = Ast.labelSetToList lbls
  in foldl initializer emptyMap listOfLabels

initializeOuts :: Ast.LabelSet -> Result
initializeOuts lbls = initializeIns lbls

kill :: Utils.Block -> Ast.Stmt -> Value
kill blk stmt =
  case blk of
    Utils.BlockStmtAssign _ v _ ->
      let aexps = Utils.removeTrivialAexps (Utils.aexpsOf stmt)
          hasVar var e =
            let vars = Utils.freeVarsOfAexp Ast.emptyVarSet e
            in Ast.memberOfVarSet vars var
      in Utils.filterAexpSet (hasVar v) aexps
    _ -> Utils.emptyAexpSet

gen :: Utils.Block -> Value
gen blk =
  case blk of
    Utils.BlockStmtAssign _ v aexp ->
      let aexps = Utils.removeTrivialAexps
            (Utils.aexpsOfAexp (Utils.mkAexpSet [aexp]) aexp)
          notHasVar var e =
            let vars = Utils.freeVarsOfAexp Ast.emptyVarSet e
            in Ast.notMemberOfVarSet vars var
      in Utils.filterAexpSet (notHasVar v) aexps
    Utils.BlockBexp _ b ->
      Utils.removeTrivialAexps
        (Utils.aexpsOfBexp Utils.emptyAexpSet b)
    _ -> Utils.emptyAexpSet

computeEntry :: Ctx -> Ast.Label -> Value
computeEntry ctx lbl =
  let stmt = Ctx.prog ctx
  in if Utils.initialLabelOf stmt == lbl then Utils.emptyAexpSet
  else
    let flows = Utils.flowSetToList (Utils.flowOf stmt)
        inflows =
          [ computeExit ctx l1 | Utils.Flow (l1, l2) <- flows, l2 == lbl ]
    in Utils.intersectionAexpSets inflows

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
  in Utils.unionAexpSets [Utils.differenceAexpSets entry killed, generated]
