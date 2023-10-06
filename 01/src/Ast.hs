module Ast where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Map = Map.Map
type Set = Set.Set


-- * Basic types (type aliases for convenience)

type Var = String -- ^ Variables
type Numb = Int -- ^ Integer numbers
type Label = Int -- ^ Pragram labels
type Flow = (Label, Label) -- ^ One label flows into another

type VarSet = Set Var
type LabelSet = Set Label
type FlowSet = Set Flow



-- * Arithmetic expressions

data Aexp =
    Avar Var
  | Anumb Numb
  | Aplus Aexp Aexp
  | Amult Aexp Aexp
  | Aminus Aexp Aexp
  deriving (Eq, Ord)

instance Show Aexp where
  show (Avar v) = v
  show (Anumb n) = show n
  show (Aplus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Amult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Aminus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"


-- * Boolean expressions

data Bexp =
    Btrue
  | Bfalse
  | Bnot Bexp
  | BeqA Aexp Aexp
  | BgtA Aexp Aexp
  deriving (Eq, Ord)

instance Show Bexp where
  show Btrue = "true"
  show Bfalse = "false"
  show (Bnot e) = "not(" ++ show e ++ ")"
  show (BeqA e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
  show (BgtA e1 e2) = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"


-- * Statements (a program is a statement)

data Stmt =
    Skip Label
  | Assign Label Var Aexp
  | Seq Stmt Stmt
  | If Label Bexp Stmt Stmt
  | While Label Bexp Stmt

instance Show Stmt where
  show (Skip l) = "[ skip ]@" ++ show l
  show (Assign l v e) = "[ " ++ v ++ " := " ++ show e ++ " ]@" ++ show l
  show (Seq s1 s2) = show s1 ++ "; " ++ show s2
  show (If l b s1 s2) =
    "if [ " ++ show b ++ " ]@" ++ show l ++
    "then { " ++ show s1 ++ " } " ++
    "else { " ++ show s2 ++ " }"
  show (While l b s) =
    "while [ " ++ show b ++ " ]@" ++ show l ++ " do { " ++ show s ++ " }"


-- * Stmt Map

type LabelMap = Map Label Stmt

buildLabelMap :: LabelMap -> Stmt -> LabelMap
buildLabelMap labMap stmt =
  case stmt of
    Skip l -> Map.insert l stmt labMap
    Assign l _ _ -> Map.insert l stmt labMap
    If l _ s1 s2 ->
      let s1Map = buildLabelMap labMap s1
          s2Map = buildLabelMap s1Map s2
      in Map.insert l stmt s2Map
    While l _ s ->
      let sMap = buildLabelMap labMap s
      in Map.insert l stmt sMap
    Seq s1 s2 ->
      let s1Map = buildLabelMap labMap s1
      in buildLabelMap s1Map s2

labelMapOf :: Stmt -> LabelMap
labelMapOf stmt = buildLabelMap (Map.empty :: LabelMap) stmt


-- * Free variables (variables mentioned in the program)

varsOfAexp :: VarSet -> Aexp -> VarSet
varsOfAexp vs e =
  case e of
    Avar v -> Set.union vs (Set.singleton v :: VarSet)
    Anumb _ -> vs
    Aplus e1 e2 -> Set.union (varsOfAexp vs e1) (varsOfAexp vs e2)
    Amult e1 e2 -> Set.union (varsOfAexp vs e1) (varsOfAexp vs e2)
    Aminus e1 e2 -> Set.union (varsOfAexp vs e1) (varsOfAexp vs e2)

varsOfBexp :: VarSet -> Bexp -> VarSet
varsOfBexp vs b =
  case b of
    Btrue -> vs
    Bfalse -> vs
    Bnot b1 -> varsOfBexp vs b1
    BeqA e1 e2 -> Set.union (varsOfAexp vs e1) (varsOfAexp vs e2)
    BgtA e1 e2 -> Set.union (varsOfAexp vs e1) (varsOfAexp vs e2)

varsOfStmt :: VarSet -> Stmt -> VarSet
varsOfStmt vs stmt =
  case stmt of
    Skip _ -> vs
    Assign _ v e -> Set.union (Set.singleton v :: VarSet) (varsOfAexp vs e)
    If _ b s1 s2 ->
      Set.unions [varsOfBexp vs b, varsOfStmt vs s1, varsOfStmt vs s2]
    While _ b s -> Set.unions [varsOfBexp vs b, varsOfStmt vs s]
    Seq s1 s2 -> Set.union (varsOfStmt vs s1) (varsOfStmt vs s2)

freeVars :: Stmt -> VarSet
freeVars stmt = varsOfStmt (Set.empty :: VarSet) stmt


-- * Initial and final labels (i.e., the entry/exit label(s) to a statement)

initial :: Stmt -> Label
initial stmt =
  case stmt of
    Skip l -> l
    Assign l _ _ -> l
    If l _ _ _ -> l
    While l _ _ -> l
    Seq s1 _ -> initial s1

final :: Stmt -> LabelSet
final stmt =
  case stmt of
    Skip l -> Set.singleton l
    Assign l _ _ -> Set.singleton l
    If _ _ s1 s2 ->
      let labels1 = final s1
          labels2 = final s2
      in Set.union labels1 labels2
    While l _ _ -> Set.singleton l
    Seq _ s2 -> final s2


-- * Blocks (pieces of the program that have a label)

data Block =
    Test Label Bexp
  | SkipBlock Label
  | AssignBlock Label Var Aexp
  deriving (Eq, Ord)

type BlockSet = Set Block

labelOf :: Block -> Label
labelOf blk =
  case blk of
    Test l _ -> l
    SkipBlock l -> l
    AssignBlock l _ _ -> l

blocks :: Stmt -> BlockSet
blocks stmt =
  case stmt of
    Skip l -> Set.singleton (SkipBlock l)
    Assign l v e -> Set.singleton (AssignBlock l v e)
    If l b s1 s2 ->
      let bBlock = Set.singleton (Test l b)
          s1Blocks = blocks s1
          s2Blocks = blocks s2
      in Set.unions [bBlock, s1Blocks, s2Blocks]
    While l b s ->
      let bBlock = Set.singleton (Test l b)
          sBlocks = blocks s
      in Set.union bBlock sBlocks
    Seq s1 s2 ->
      let s1Blocks = blocks s1
          s2Blocks = blocks s2
      in Set.union s1Blocks s2Blocks

labels :: Stmt -> LabelSet
labels stmt = Set.map labelOf (blocks stmt)


-- * Flows

flow :: Stmt -> FlowSet
flow stmt =
  case stmt of
    Skip _ -> Set.empty :: FlowSet
    Assign _ _ _ -> Set.empty :: FlowSet
    If l _ s1 s2 ->
      let s1Flows = flow s1
          s2Flows = flow s2
          s1init = initial s1
          s2init = initial s2
          connectors = Set.fromList [(l, s1init), (l, s2init)]
      in Set.unions [s1Flows, s2Flows, connectors]
    While l _ s ->
      let sFlows = flow s
          sInit = initial s
          sFinals = final s
          finals = Set.map (\l2 -> (l2, l)) sFinals
          connector = Set.singleton (l, sInit)
      in Set.unions [sFlows, connector, finals]
    Seq s1 s2 ->
      let s1Flows = flow s1
          s2Flows = flow s2
          s2Init = initial s2
          s1Finals = final s1
          connectors = Set.map (\l2 -> (l2, s2Init)) s1Finals
      in Set.unions [s1Flows, s2Flows, connectors]

flowR :: Stmt -> FlowSet
flowR stmt =
  let forwardFlow = flow stmt
  in Set.map (\(l1, l2) -> (l2, l1)) forwardFlow

labelsFlowingInto :: FlowSet -> Label -> LabelSet
labelsFlowingInto flowset l =
  let check (_, l1) = l1 == l
      results = Set.filter check flowset
  in Set.map (\(l2, _) -> l2) results


-- * Properties

flowInvariant :: Stmt -> Bool
flowInvariant stmt =
  let labs = labels stmt
      entrypoint = initial stmt
      flows = flow stmt
      flowIns = Set.map (\(l, _) -> l) flows
      flowOuts = Set.map (\(_, l) -> l) flows
      labs2 = Set.unions [Set.singleton entrypoint, flowIns, flowOuts]
  in labs == labs2

flowRInvariant :: Stmt -> Bool
flowRInvariant stmt =
  let labs = labels stmt
      exitpoints = final stmt
      flows = flow stmt
      flowIns = Set.map (\(l, _) -> l) flows
      flowOuts = Set.map (\(_, l) -> l) flows
      labs2 = Set.unions [exitpoints, flowIns, flowOuts]
  in labs == labs2

hasIsolatedEntries :: Stmt -> Bool
hasIsolatedEntries stmt =
  let entrypoint = initial stmt
      labs = labels stmt
      flows = flow stmt
      check lab = Set.member (lab, entrypoint) flows
      result = Set.filter check labs
  in Set.size result == 0

hasIsolatedExits :: Stmt -> Bool
hasIsolatedExits stmt =
  let exitpoints = final stmt
      labs = labels stmt
      flowsR = flowR stmt
      pairs = Set.cartesianProduct exitpoints labs
      check (l1, l2) = Set.member (l1, l2) flowsR
      result = Set.filter check pairs
  in Set.size result == 0
