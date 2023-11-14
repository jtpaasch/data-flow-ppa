{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Equations where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast

type Map = Map.Map
type Set = Set.Set


-- * Reaching definitions

-- A program variable that occurs in a program
type Var = Ast.Var

-- The label where a variable was defined (i.e., assigned a value).
data DefLabel =
    Label Ast.Label -- ^ A particular program label.
  | Unknown -- ^ We don't know the program label in question.
  deriving (Eq, Ord)

instance Show DefLabel where
  show (Label l) = show l
  show Unknown = "?"

-- Convert a label from the AST to a 'DefLabel'.
toDefLabel :: Ast.Label -> DefLabel
toDefLabel lbl = Label lbl

-- A reaching definition consists of a variable and a program label.
-- A '(v, l)' pair indicates the label 'l' where the variable 'v' was
-- assigned its current value.
type RD = (Var, DefLabel)

-- A set of reaching definitions.
type RDset = Set RD

-- A set of reaching definitions associated with a program label.
type LabeledRDset = (Ast.Label, RDset)

-- The reaching definitions ('RDs' for short) for a program
-- are comprised of a set of RDs for each program label.
type RDs = Map Ast.Label RDset

-- Track RDs separately for the entrance and exit to/of a program label.
type EntryRDs = RDs
type ExitRDs = RDs

mkRD :: Var -> DefLabel -> RD
mkRD var lbl = (var, lbl)

mkUnknownRD :: Var -> RD
mkUnknownRD var = (var, Unknown)

mkLabeledRDset :: Ast.Label -> RDset -> LabeledRDset
mkLabeledRDset lbl rdset = (lbl, rdset)

mkLabeledEmptyRDset :: Ast.Label -> LabeledRDset
mkLabeledEmptyRDset lbl = (lbl, Set.empty :: RDset)

getRDset :: RDs -> Ast.Label -> RDset
getRDset rds key =
  case Map.lookup key rds of
    Nothing -> undefined
    Just value -> value

updateRDs :: RDs -> Ast.Label -> RDset -> RDs
updateRDs rds key value = Map.update (\ _ -> Just value) key rds


-- * Algorithm

data Ctx = Ctx
  { changed :: Bool
  , ins :: RDs
  , outs :: RDs
  , prog :: Ast.Stmt
  , stmts :: Ast.LabelMap
  , labels :: [Ast.Label]
  , firstLabel :: Ast.Label
  , initialRDset :: RDset
  , flows :: Ast.FlowSet
  } deriving Show

initialEntries :: Ast.Stmt -> RDset
initialEntries stmt =
  let vars = Ast.freeVars stmt
  in Set.map mkUnknownRD vars

emptyRDs :: Ast.LabelMap -> RDs
emptyRDs labelMap =
  let lbls = Map.keys labelMap
      entries = map mkLabeledEmptyRDset lbls
  in Map.fromList entries

getEntrySeed :: Ctx -> Ast.Label -> RDset
getEntrySeed ctx lbl =
  if lbl == firstLabel ctx
    then initialRDset ctx -- first label entry is already seeded
    else Set.empty :: RDset

getEntrySet :: Ctx -> Ast.Label -> RDset
getEntrySet ctx lbl =
  let exitRDs = outs ctx
      flowset = flows ctx
      seed = getEntrySeed ctx lbl
      entryLabels = Ast.labelsFlowingInto flowset lbl
      entrySets = Set.map (\lbl2 -> getRDset exitRDs lbl2) entryLabels
  in Set.union seed (Set.unions entrySets)

allPossibleDefsOfVar :: Ctx -> Ast.Var -> RDset
allPossibleDefsOfVar ctx var =
  let lbls = Set.fromList (labels ctx)
      possibleLabeledDefs = Set.map (\lbl -> mkRD var (toDefLabel lbl)) lbls
      possibleUnknownDef = Set.singleton (mkUnknownRD var)
  in Set.union possibleUnknownDef possibleLabeledDefs

getStmtOfLabel :: Ctx -> Ast.Label -> Ast.Stmt
getStmtOfLabel ctx lbl =
  let labelMap = stmts ctx
  in case Map.lookup lbl labelMap of
    Nothing -> undefined
    Just stmt -> stmt

doUpdate :: Ctx -> Ast.Label -> (RDset, RDset)
doUpdate ctx lbl =
  let entrySet = getEntrySet ctx lbl -- RDs coming into lbl
      stmt = getStmtOfLabel ctx lbl
  in case stmt of
    Ast.Assign _ v _ ->
      let oldDefs = allPossibleDefsOfVar ctx v
          rdset = Set.difference entrySet oldDefs -- remove any defs of v
          newDef = mkRD v (toDefLabel lbl) -- this is a new def of v
          exitSet = Set.union rdset (Set.singleton newDef) -- add in new def
      in (entrySet, exitSet)
    _ -> (entrySet, entrySet)

updateAnalysis :: Ctx -> Ast.Label -> Ctx
updateAnalysis ctx lbl =
  let entryRDs = ins ctx
      exitRDs = outs ctx
      origEntry = getRDset entryRDs lbl
      origExit = getRDset exitRDs lbl
      (updatedEntry, updatedExit) = doUpdate ctx lbl
  in if (origEntry == updatedEntry) && (origExit == updatedExit)
    then ctx
    else
      let entryRDs2 = updateRDs entryRDs lbl updatedEntry
          exitRDs2 = updateRDs exitRDs lbl updatedExit
      in ctx { changed = True, ins = entryRDs2, outs = exitRDs2 }

doIterate :: Ctx -> Ctx
doIterate ctx =
  let ctx2 = ctx { changed = False }
      labelMap = stmts ctx
      lbls = Map.keys labelMap
      ctx3 = Prelude.foldl updateAnalysis ctx2 lbls
  in if changed ctx3
    then doIterate ctx3
    else ctx3

roundRobin :: Ast.Stmt -> Ctx
roundRobin stmt =
  let labelMap = Ast.labelMapOf stmt
      lbls = Map.keys labelMap
      firstLab = head lbls
      initRDset = initialEntries stmt
      initEntryRDs = emptyRDs labelMap
      entryRDs = updateRDs initEntryRDs firstLab initRDset
      exitRDs = emptyRDs labelMap
      flowset = Ast.flow stmt
      ctx = Ctx
        { changed = True
        , ins = entryRDs
        , outs = exitRDs
        , prog = stmt
        , stmts = labelMap
        , labels = lbls
        , firstLabel = firstLab
        , initialRDset = initRDset
        , flows = flowset
        }
  in doIterate ctx
