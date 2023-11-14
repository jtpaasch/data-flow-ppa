module Analysis where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Utils as Utils

type Map = Map.Map
type Set = Set.Set

type Value = Utils.AexpSet

type Result = Map Ast.Label Value

data Ctx = Ctx
  { prog :: Ast.Stmt
  , labels :: Ast.LabelSet
  , cursor :: Ast.Label
  , ins :: Result
  , outs :: Result
  } deriving Show

getVal :: Ast.Label -> Result -> Value
getVal lbl values =
  case Map.lookup lbl values of
    Nothing -> undefined
    Just value -> value

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

initializeIns :: Ast.LabelSet -> Result
initializeIns lbls =
  let initializer = \result key -> Map.insert key Utils.emptyAexpSet result
      emptyMap = Map.empty :: Result
      listOfLabels = Ast.labelSetToList lbls
  in foldl initializer emptyMap listOfLabels

initializeOuts :: Ast.LabelSet -> Result
initializeOuts lbls = initializeIns lbls

computeEntry :: Ctx -> Ast.Label -> Value
computeEntry ctx lbl =
  let stmt = prog ctx
  in if Utils.initialLabelOf stmt == lbl then Utils.emptyAexpSet
  else
    let flows = Utils.flowSetToList (Utils.flowOf stmt)
        inflows =
          [ computeExit ctx l1 | Utils.Flow (l1, l2) <- flows, l2 == lbl ]
    in Utils.intersectionAexpSets inflows

computeExit :: Ctx -> Ast.Label -> Value
computeExit ctx lbl =
  let stmt = prog ctx
      blocks = Utils.blocksOf stmt
      blockOf l blks = case blks of
        [] -> undefined
        x : xs ->
          if Utils.labelOfBlock x == l then x
          else blockOf l xs
      blk = blockOf lbl (Utils.blockSetToList blocks)
      entry = getVal lbl (ins ctx)
      killed = kill blk stmt
      generated = gen blk
  in Utils.unionAexpSets [Utils.differenceAexpSets entry killed, generated]

doComputations :: Ctx -> Ctx
doComputations ctx =
  let lbl = cursor ctx
      entries = ins ctx
      newEntryValue = computeEntry ctx lbl
      newEntries = Map.update (\_ -> Just newEntryValue) lbl entries
      ctx2 = ctx { ins = newEntries }
      exits = outs ctx2
      newExitValue = computeExit ctx2 lbl
      newExits = Map.update (\_ -> Just newExitValue) lbl exits
  in ctx2 { outs = newExits }

updateCursor :: Ctx -> Ctx
updateCursor ctx =
  let lbls = Ast.labelSetToList (labels ctx)
      lbl = cursor ctx
  in case List.elemIndex lbl lbls of
    Nothing -> undefined
    Just i ->
      let (_, rest) = List.splitAt i lbls
          newCursor = case rest of
            [] -> head lbls
            _ : [] -> head lbls
            _ : xs -> head xs
      in ctx { cursor = newCursor }

doIterate :: Ctx -> Ctx
doIterate ctx =
  let ctx2 = doComputations ctx
  in if (ins ctx /= ins ctx2) || (outs ctx /= outs ctx2)
    then
      let ctx3 = updateCursor ctx2
      in doIterate ctx3
    else ctx2

analyze :: Ast.Stmt -> Ctx
analyze stmt =
  let lbls = Utils.labelsOf stmt
      ctx = Ctx
        { prog = stmt
        , labels = lbls
        , cursor = Utils.initialLabelOf stmt
        , ins = initializeIns lbls
        , outs = initializeOuts lbls
        }
  in doIterate ctx
