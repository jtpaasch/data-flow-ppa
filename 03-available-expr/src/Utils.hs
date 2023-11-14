module Utils where

import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Ast as Ast

type Set = Set.Set


-- * Printing

showSet :: Show a => [a] -> String
showSet xs =
  let strings = map show xs
  in "{" ++ (intercalate "," strings) ++ "}"


-- * Free vars

freeVarsOfAexp :: Ast.VarSet -> Ast.Aexp -> Ast.VarSet
freeVarsOfAexp vars e =
  case e of
    Ast.AexpVar v -> Ast.addToVarSet vars v
    Ast.AexpNumb _ -> vars
    Ast.AexpPlus e1 e2 ->
      let vars2 = freeVarsOfAexp vars e1
      in freeVarsOfAexp vars2 e2
    Ast.AexpMult e1 e2 ->
      let vars2 = freeVarsOfAexp vars e1
      in freeVarsOfAexp vars2 e2
    Ast.AexpMinus e1 e2 ->
      let vars2 = freeVarsOfAexp vars e1
      in freeVarsOfAexp vars2 e2

freeVarsOfBexp :: Ast.VarSet -> Ast.Bexp -> Ast.VarSet
freeVarsOfBexp vars b =
  case b of
    Ast.BexpTrue -> vars
    Ast.BexpFalse -> vars
    Ast.BexpNot b2 -> freeVarsOfBexp vars b2
    Ast.BexpEqA e1 e2 ->
      let vars2 = freeVarsOfAexp vars e1
      in freeVarsOfAexp vars2 e2
    Ast.BexpLtA e1 e2 ->
      let vars2 = freeVarsOfAexp vars e1
      in freeVarsOfAexp vars2 e2

freeVarsOfStmt :: Ast.VarSet -> Ast.Stmt -> Ast.VarSet
freeVarsOfStmt vars stmt =
  case stmt of
    Ast.StmtSkip _ -> vars
    Ast.StmtAssign _ v e ->
      let vars2 = Ast.addToVarSet vars v
      in freeVarsOfAexp vars2 e
    Ast.StmtIf _ b stmt1 stmt2 ->
      let vars2 = freeVarsOfBexp vars b
          vars3 = freeVarsOfStmt vars2 stmt1
      in freeVarsOfStmt vars3 stmt2
    Ast.StmtWhile _ b body ->
      let vars2 = freeVarsOfBexp vars b
      in freeVarsOfStmt vars2 body
    Ast.StmtSeq stmt1 stmt2 ->
      let vars2 = freeVarsOfStmt vars stmt1
      in freeVarsOfStmt vars2 stmt2

freeVars :: Ast.Stmt -> Ast.VarSet
freeVars stmt = freeVarsOfStmt Ast.emptyVarSet stmt


-- * Initial and Final Labels

initialLabelOf :: Ast.Stmt -> Ast.Label
initialLabelOf stmt =
  case stmt of
    Ast.StmtSkip l -> l
    Ast.StmtAssign l _ _ -> l
    Ast.StmtIf l _ _ _ -> l
    Ast.StmtWhile l _ _ -> l
    Ast.StmtSeq stmt1 _ -> initialLabelOf stmt1

finalLabelsOf :: Ast.Stmt -> Ast.LabelSet
finalLabelsOf stmt =
  case stmt of
    Ast.StmtSkip l -> Ast.mkLabelSet [l]
    Ast.StmtAssign l _ _ -> Ast.mkLabelSet [l]
    Ast.StmtIf _ _ stmt1 stmt2 ->
      Ast.unionLabelSets [finalLabelsOf stmt1, finalLabelsOf stmt2]
    Ast.StmtWhile l _ _ -> Ast.mkLabelSet [l]
    Ast.StmtSeq _ stmt2 -> finalLabelsOf stmt2


-- * "Blocks" (i.e., pieces of syntax that have labels)

data Block =
    BlockBexp Ast.Label Ast.Bexp
  | BlockStmtSkip Ast.Label
  | BlockStmtAssign Ast.Label Ast.Var Ast.Aexp
  deriving (Eq, Ord)

instance Show Block where
  show (BlockBexp l b) = "[ " ++ show b ++ " ]@" ++ show l
  show (BlockStmtSkip l) = show (Ast.StmtSkip l)
  show (BlockStmtAssign l v e) = show (Ast.StmtAssign l v e)

data BlockSet = BlockSet (Set Block) deriving (Eq, Ord)

instance Show BlockSet where
  show (BlockSet xs) = showSet $ Set.toList xs

blockSetToList :: BlockSet -> [Block]
blockSetToList (BlockSet xs) = Set.toList xs

emptyBlockSet :: BlockSet
emptyBlockSet = BlockSet Set.empty

mkBlockSet :: [Block] -> BlockSet
mkBlockSet blks = BlockSet $ Set.fromList blks

addToBlockSet :: BlockSet -> Block -> BlockSet
addToBlockSet (BlockSet blocks) block = BlockSet (Set.insert block blocks)

unionBlockSets :: [BlockSet] -> BlockSet
unionBlockSets blocksets =
  let union (BlockSet blocks1) (BlockSet blocks2) =
        BlockSet (Set.union blocks1 blocks2)
  in foldl union emptyBlockSet blocksets

labelOfBlock :: Block -> Ast.Label
labelOfBlock block =
  case block of
    BlockBexp l _ -> l
    BlockStmtSkip l -> l
    BlockStmtAssign l _ _ -> l

blocksOf :: Ast.Stmt -> BlockSet
blocksOf stmt =
  case stmt of
    Ast.StmtSkip l -> mkBlockSet [BlockStmtSkip l]
    Ast.StmtAssign l v e -> mkBlockSet [BlockStmtAssign l v e]
    Ast.StmtIf l b stmt1 stmt2 ->
      let blocks1 = mkBlockSet [BlockBexp l b]
          blocks2 = blocksOf stmt1
          blocks3 = blocksOf stmt2
      in unionBlockSets [blocks1, blocks2, blocks3]
    Ast.StmtWhile l b body ->
      let blocks1 = mkBlockSet [BlockBexp l b]
          blocks2 = blocksOf body
      in unionBlockSets [blocks1, blocks2]
    Ast.StmtSeq stmt1 stmt2 ->
      let blocks1 = blocksOf stmt1
          blocks2 = blocksOf stmt2
      in unionBlockSets [blocks1, blocks2]

labelsOf :: Ast.Stmt -> Ast.LabelSet
labelsOf stmt =
  let blocks = blocksOf stmt
      labels = map labelOfBlock (blockSetToList blocks)
  in Ast.mkLabelSet labels


-- * The arithmetic expressions that occur in a program

data AexpSet = AexpSet (Set Ast.Aexp) deriving (Eq, Ord)

instance Show AexpSet where
  show (AexpSet xs) = showSet $ Set.toList xs

aexpSetToList :: AexpSet -> [Ast.Aexp]
aexpSetToList (AexpSet xs) = Set.toList xs

emptyAexpSet :: AexpSet
emptyAexpSet = AexpSet Set.empty

mkAexpSet :: [Ast.Aexp] -> AexpSet
mkAexpSet aexps = AexpSet $ Set.fromList aexps

addToAexpSet :: AexpSet -> Ast.Aexp -> AexpSet
addToAexpSet (AexpSet aexps) aexp = AexpSet (Set.insert aexp aexps)

unionAexpSets :: [AexpSet] -> AexpSet
unionAexpSets aexpsets =
  let union (AexpSet aexps1) (AexpSet aexps2) =
        AexpSet (Set.union aexps1 aexps2)
  in foldl union emptyAexpSet aexpsets

intersectionAexpSets :: [AexpSet] -> AexpSet
intersectionAexpSets aexpsets =
  let intersection (AexpSet aexps1) (AexpSet aexps2) =
        AexpSet (Set.intersection aexps1 aexps2)
  in Prelude.foldl intersection (head aexpsets) aexpsets

differenceAexpSets :: AexpSet -> AexpSet -> AexpSet
differenceAexpSets (AexpSet aexpset1) (AexpSet aexpset2) =
  AexpSet (Set.difference aexpset1 aexpset2)

filterAexpSet :: (Ast.Aexp -> Bool) -> AexpSet -> AexpSet
filterAexpSet f (AexpSet xs) = AexpSet (Set.filter f xs)

aexpsOfAexp :: AexpSet -> Ast.Aexp -> AexpSet
aexpsOfAexp aexps e =
  case e of
    Ast.AexpVar _ -> unionAexpSets [aexps, mkAexpSet [e]]
    Ast.AexpNumb _ -> unionAexpSets [aexps, mkAexpSet [e]]
    Ast.AexpPlus e1 e2 ->
      let aexps1 = aexpsOfAexp aexps e1
          aexps2 = aexpsOfAexp aexps e2
      in unionAexpSets [aexps, aexps1, aexps2]
    Ast.AexpMult e1 e2 ->
      let aexps1 = aexpsOfAexp aexps e1
          aexps2 = aexpsOfAexp aexps e2
      in unionAexpSets [aexps, aexps1, aexps2]
    Ast.AexpMinus e1 e2 ->
      let aexps1 = aexpsOfAexp aexps e1
          aexps2 = aexpsOfAexp aexps e2
      in unionAexpSets [aexps, aexps1, aexps2]

aexpsOfBexp :: AexpSet -> Ast.Bexp -> AexpSet
aexpsOfBexp aexps b =
  case b of
    Ast.BexpTrue -> aexps
    Ast.BexpFalse -> aexps
    Ast.BexpNot b2 ->
      let aexps1 = aexpsOfBexp aexps b2
      in unionAexpSets [aexps, aexps1]
    Ast.BexpEqA e1 e2 ->
      let aexps1 = aexpsOfAexp aexps e1
          aexps2 = aexpsOfAexp aexps e2
      in unionAexpSets [aexps, aexps1, aexps2]
    Ast.BexpLtA e1 e2 ->
      let aexps1 = aexpsOfAexp aexps e1
          aexps2 = aexpsOfAexp aexps e2
      in unionAexpSets [aexps, aexps1, aexps2]

aexpsOfStmt :: AexpSet -> Ast.Stmt -> AexpSet
aexpsOfStmt aexps stmt =
  case stmt of
    Ast.StmtSkip _ -> aexps
    Ast.StmtAssign _ _ e ->
      let aexps1 = aexpsOfAexp aexps e
      in unionAexpSets [aexps, mkAexpSet [e], aexps1]
    Ast.StmtIf _ bexp stmt1 stmt2 ->
      let aexps1 = aexpsOfStmt aexps stmt1
          aexps2 = aexpsOfStmt aexps stmt2
          aexps3 = aexpsOfBexp aexps bexp
      in unionAexpSets [aexps, aexps1, aexps2, aexps3]
    Ast.StmtWhile _ bexp body ->
      let aexps1 = aexpsOfStmt aexps body
          aexps2 = aexpsOfBexp aexps bexp
      in unionAexpSets [aexps, aexps1, aexps2]
    Ast.StmtSeq stmt1 stmt2 ->
      let aexps1 = aexpsOfStmt aexps stmt1
          aexps2 = aexpsOfStmt aexps stmt2
      in unionAexpSets [aexps, aexps1, aexps2]

aexpsOf :: Ast.Stmt -> AexpSet
aexpsOf stmt = aexpsOfStmt emptyAexpSet stmt

removeTrivialAexps :: AexpSet -> AexpSet
removeTrivialAexps aexps =
  let check aexp =
        case aexp of
          Ast.AexpPlus _ _ -> True
          Ast.AexpMult _ _ -> True
          Ast.AexpMinus _ _ -> True
          _ -> False
  in filterAexpSet check aexps


-- * "Flows" (i.e., program flow from one label to another)

data Flow = Flow (Ast.Label, Ast.Label) deriving (Eq, Ord)

instance Show Flow where
  show (Flow x) = show x

mkFlow :: Ast.Label -> Ast.Label -> Flow
mkFlow l1 l2 = Flow (l1, l2)

data FlowSet = FlowSet (Set Flow) deriving (Eq, Ord)

instance Show FlowSet where
  show (FlowSet xs) = showSet $ Set.toList xs

flowSetToList :: FlowSet -> [Flow]
flowSetToList (FlowSet xs) = Set.toList xs

emptyFlowSet :: FlowSet
emptyFlowSet = FlowSet Set.empty

mkFlowSet :: [Flow] -> FlowSet
mkFlowSet flows = FlowSet $ Set.fromList flows

addToFlowSet :: FlowSet -> Flow -> FlowSet
addToFlowSet (FlowSet flows) flow = FlowSet (Set.insert flow flows)

unionFlowSets :: [FlowSet] -> FlowSet
unionFlowSets flowsets =
  let union (FlowSet flows1) (FlowSet flows2) =
        FlowSet (Set.union flows1 flows2)
  in foldl union emptyFlowSet flowsets

isInFlowSet :: Flow -> FlowSet -> Bool
isInFlowSet flow (FlowSet flows) = Set.member flow flows

flowOf :: Ast.Stmt -> FlowSet
flowOf stmt =
  case stmt of
    Ast.StmtSkip _ -> emptyFlowSet
    Ast.StmtAssign _ _ _ -> emptyFlowSet
    Ast.StmtIf l _ stmt1 stmt2 ->
      let flows1 = flowOf stmt1
          flows2 = flowOf stmt2
          thenFlow = mkFlowSet [mkFlow l (initialLabelOf stmt1)]
          elseFlow = mkFlowSet [mkFlow l (initialLabelOf stmt2)]
      in unionFlowSets [flows1, flows2, thenFlow, elseFlow]
    Ast.StmtWhile l _ body ->
      let flows = flowOf body
          inFlow = mkFlowSet [mkFlow l (initialLabelOf body)]
          outLabels = Ast.labelSetToList (finalLabelsOf body)
          connectors = [ mkFlow outLabel l | outLabel <- outLabels ]
      in unionFlowSets [flows, inFlow, mkFlowSet connectors]
    Ast.StmtSeq stmt1 stmt2 ->
      let flows1 = flowOf stmt1
          flows2 = flowOf stmt2
          inLabel = initialLabelOf stmt2
          outLabels = Ast.labelSetToList (finalLabelsOf stmt1)
          connectors = [ mkFlow outLabel inLabel | outLabel <- outLabels ]
      in unionFlowSets [flows1, flows2, mkFlowSet connectors]

reverseFlowOf :: Ast.Stmt -> FlowSet
reverseFlowOf stmt =
  let forwardFlows = flowSetToList (flowOf stmt)
      reverseFlows = [ mkFlow l2 l1 | Flow (l1, l2) <- forwardFlows ]
  in mkFlowSet reverseFlows
