module Ast where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Pretty as Pretty

type Map = Map.Map
type Set = Set.Set



-- * Basic types

-- Variables
data Var = Var String deriving (Eq, Ord)

instance Show Var where
  show (Var x) = x


-- Literal numbers
data Numb = Numb Int deriving (Eq, Ord)

instance Show Numb where
  show (Numb i) = show i


-- Program labels
data Label = Label Int deriving (Eq, Ord)

instance Show Label where
  show (Label l) = show l


-- A set of variables
data VarSet = VarSet (Set Var) deriving (Eq, Ord)

instance Show VarSet where
  show (VarSet xs) = Pretty.showSet $ Set.toList xs

varSetToList :: VarSet -> [Var]
varSetToList (VarSet xs) = Set.toList xs

emptyVarSet :: VarSet
emptyVarSet = VarSet Set.empty

mkVarSet :: [Var] -> VarSet
mkVarSet xs = VarSet $ Set.fromList xs

addToVarSet :: VarSet -> Var -> VarSet
addToVarSet (VarSet vars) v = VarSet (Set.insert v vars)


-- A set of program labels
data LabelSet = LabelSet (Set Label) deriving (Eq, Ord)

instance Show LabelSet where
  show (LabelSet xs) = Pretty.showSet $ Set.toList xs

labelSetToList :: LabelSet -> [Label]
labelSetToList (LabelSet xs) = Set.toList xs

emptyLabelSet :: LabelSet
emptyLabelSet = LabelSet Set.empty

mkLabelSet :: [Label] -> LabelSet
mkLabelSet xs = LabelSet $ Set.fromList xs

addToLabelSet :: LabelSet -> Label -> LabelSet
addToLabelSet (LabelSet labels) l = LabelSet (Set.insert l labels)

unionLabelSets :: [LabelSet] -> LabelSet
unionLabelSets labelsets =
  let union (LabelSet labels1) (LabelSet labels2) =
        LabelSet (Set.union labels1 labels2)
  in foldl union emptyLabelSet labelsets


-- * Arithmetic expressions

data Aexp =
    AexpVar Var
  | AexpNumb Numb
  | AexpPlus Aexp Aexp
  | AexpMult Aexp Aexp
  | AexpMinus Aexp Aexp
  deriving (Eq, Ord)

instance Show Aexp where
  show (AexpVar v) = show v
  show (AexpNumb n) = show n
  show (AexpPlus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (AexpMult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (AexpMinus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"


-- * Boolean expressions

data Bexp =
    BexpTrue
  | BexpFalse
  | BexpNot Bexp
  | BexpEqA Aexp Aexp
  | BexpLtA Aexp Aexp
  deriving (Eq, Ord)

instance Show Bexp where
  show BexpTrue = "true"
  show BexpFalse = "false"
  show (BexpNot e) = "not(" ++ show e ++ ")"
  show (BexpEqA e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
  show (BexpLtA e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"


-- * Statements (note that a program is a statement)

data Stmt =
    StmtSkip Label
  | StmtAssign Label Var Aexp
  | StmtIf Label Bexp Stmt Stmt
  | StmtWhile Label Bexp Stmt
  | StmtSeq Stmt Stmt
  
instance Show Stmt where
  show (StmtSkip l) =
    "[ skip ]@" ++ show l
  show (StmtAssign l v e) =
    "[ " ++ show v ++ " := " ++ show e ++ " ]@" ++ show l
  show (StmtIf l b thenStmt elseStmt) =
    "if [ " ++ show b ++ " ]@" ++ show l ++
    "then { " ++ show thenStmt ++ " }" ++
    "else { " ++ show elseStmt ++ " }"
  show (StmtWhile l b body) =
    "while [ " ++ show b ++ " ]@" ++ show l ++ " do " ++
    "{ " ++ show body ++ " }"
  show (StmtSeq stmt1 stmt2) =
    show stmt1 ++ "; " ++ show stmt2
