module Context where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Utils as Utils

type Map = Map.Map
type Set = Set.Set

-- For printing sets.
showSet :: Show a => [a] -> String
showSet xs =
  let strings = map show xs
  in "{" ++ (intercalate "," strings) ++ "}"


-- The label where a variable was last defined.
-- Either we don't know where a variable was last defined,
-- or it's a particular label in the program.
data LabelIndex =
    UnknownLabel
  | KnownLabel Ast.Label
  deriving (Eq, Ord)

instance Show LabelIndex where
  show UnknownLabel = "?"
  show (KnownLabel lbl) = show lbl


-- A 'Def' records the label where a variable was last defined.
type Def = (Ast.Var, LabelIndex)

mkDef :: Ast.Var -> LabelIndex -> Def
mkDef v l = (v, l)

varOfDef :: Def -> Ast.Var
varOfDef x = fst x

labelOfDef :: Def -> LabelIndex
labelOfDef x = snd x


-- A set of defs.
data DefSet = DefSet (Set Def) deriving (Eq, Ord)

instance Show DefSet where
  show (DefSet xs) = showSet $ Set.toList xs

defSetToList :: DefSet -> [Def]
defSetToList (DefSet xs) = Set.toList xs

emptyDefSet :: DefSet
emptyDefSet = DefSet Set.empty

mkDefSet :: [Def] -> DefSet
mkDefSet xs = DefSet $ Set.fromList xs

addToDefSet :: DefSet -> Def -> DefSet
addToDefSet (DefSet xs) x = DefSet (Set.insert x xs)

unionDefSets :: [DefSet] -> DefSet
unionDefSets defsets =
  let union (DefSet defs1) (DefSet defs2) =
        DefSet (Set.union defs1 defs2)
  in foldl union emptyDefSet defsets

differenceDefSets :: DefSet -> DefSet -> DefSet
differenceDefSets (DefSet defs1) (DefSet defs2) =
  DefSet (Set.difference defs1 defs2)


-- 'Value' and 'Result' types for the analysis.
type Value = DefSet
type Result = Map Ast.Label Value


-- The context the analysis carries around.
data Ctx = Ctx
  { prog :: Ast.Stmt -- ^ The program to analyze
  , labels :: Ast.LabelSet -- ^ The set of labels in the program
  , cursor :: Ast.Label -- ^ Which label to look at next
  , lookedAt :: [Ast.Label] -- ^ Labels already looked at
  , seenAll :: Bool -- ^ Have we seen all the labels?
  , finalCheck :: Bool -- ^ Is this the final pass over the labels?
  , ins :: Result -- ^ Results for entry to blocks 
  , outs :: Result -- ^ Results for exit from blocks
  , debugIns :: [Result] -- ^ A list of all computed ins (for debugging)
  , debugOuts :: [Result] -- ^ A list of all computed outs (for debugging)
  } deriving Show

initCtx :: Ast.Stmt -> Result -> Result -> Ctx
initCtx stmt initialIns initialOuts =
  let lbls = Utils.labelsOf stmt
  in Ctx
    { prog = stmt
    , labels = lbls
    , cursor = Utils.initialLabelOf stmt
    , lookedAt = []
    , seenAll = False
    , finalCheck = False
    , ins = initialIns
    , outs = initialOuts
    , debugIns = []
    , debugOuts = []
    }
