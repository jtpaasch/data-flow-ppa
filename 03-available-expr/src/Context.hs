module Context where

import qualified Data.Map as Map
import qualified Ast as Ast
import qualified Utils as Utils

type Map = Map.Map

type Value = Utils.AexpSet

type Result = Map Ast.Label Value

data Ctx = Ctx
  { prog :: Ast.Stmt
  , labels :: Ast.LabelSet
  , cursor :: Ast.Label
  , ins :: Result
  , outs :: Result
  } deriving Show
