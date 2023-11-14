module Runner where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Utils as Utils
import qualified Context as Ctx
import qualified Analysis as Analysis

type Map = Map.Map
type Set = Set.Set
type Ctx = Ctx.Ctx

doComputations :: Ctx -> Ctx
doComputations ctx =
  let lbl = Ctx.cursor ctx
      entries = Ctx.ins ctx
      newEntryValue = Analysis.computeEntry ctx lbl
      newEntries = Map.update (\_ -> Just newEntryValue) lbl entries
      ctx2 = ctx { Ctx.ins = newEntries }
      exits = Ctx.outs ctx2
      newExitValue = Analysis.computeExit ctx2 lbl
      newExits = Map.update (\_ -> Just newExitValue) lbl exits
  in ctx2 { Ctx.outs = newExits }

updateCursor :: Ctx -> Ctx
updateCursor ctx =
  let lbls = Ast.labelSetToList (Ctx.labels ctx)
      lbl = Ctx.cursor ctx
  in case List.elemIndex lbl lbls of
    Nothing -> undefined
    Just i ->
      let (_, rest) = List.splitAt i lbls
          newCursor = case rest of
            [] -> head lbls
            _ : [] -> head lbls
            _ : xs -> head xs
      in ctx { Ctx.cursor = newCursor }

doIterate :: Ctx -> Ctx
doIterate ctx =
  let ctx2 = doComputations ctx
  in if (Ctx.ins ctx /= Ctx.ins ctx2) || (Ctx.outs ctx /= Ctx.outs ctx2)
    then
      let ctx3 = updateCursor ctx2
      in doIterate ctx3
    else ctx2

analyze :: Ast.Stmt -> Ctx
analyze stmt =
  let lbls = Utils.labelsOf stmt
      ctx = Ctx.Ctx
        { Ctx.prog = stmt
        , Ctx.labels = lbls
        , Ctx.cursor = Utils.initialLabelOf stmt
        , Ctx.ins = Analysis.initializeIns lbls
        , Ctx.outs = Analysis.initializeOuts lbls
        }
  in doIterate ctx
