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
      newDebugEntries = Ctx.debugIns ctx ++ [newEntries]
      ctx2 = ctx { Ctx.ins = newEntries, Ctx.debugIns = newDebugEntries }
      exits = Ctx.outs ctx2
      newExitValue = Analysis.computeExit ctx2 lbl
      newExits = Map.update (\_ -> Just newExitValue) lbl exits
      newDebugExits = Ctx.debugOuts ctx ++ [newExits]
  in ctx2 { Ctx.outs = newExits, Ctx.debugOuts = newDebugExits }

updateCursor :: Ctx -> Ctx
updateCursor ctx =
  let lbls = Ast.labelSetToList (Ctx.labels ctx)
      lbl = Ctx.cursor ctx
      seen = Ctx.lookedAt ctx
      headOf xs = case xs of
        [] -> undefined
        x : _ -> x
  in case List.elemIndex lbl lbls of
    Nothing -> undefined
    Just i ->
      let (_, rest) = List.splitAt i lbls
          newCursor = case rest of
            [] -> headOf lbls
            _ : [] -> headOf lbls
            _ : xs -> headOf xs
      in if Ctx.finalCheck ctx
        then -- If we're doing the final pass, track what we've seen so far
          let alreadySeen = seen ++ [newCursor]
              notSeen = filter (\l -> List.notElem l alreadySeen) lbls
              hasSeenAll = List.length notSeen == 0
          in ctx
            { Ctx.cursor = newCursor
            , Ctx.lookedAt = alreadySeen
            , Ctx.seenAll = hasSeenAll
            }
        else
          ctx { Ctx.cursor = newCursor }

doIterate :: Ctx -> Ctx
doIterate ctx =
  let ctx2 = doComputations ctx
  in if (Ctx.ins ctx /= Ctx.ins ctx2) || (Ctx.outs ctx /= Ctx.outs ctx2)
    then -- Something changed, so reset any final checking
      let ctx3 = updateCursor ctx2
          ctx4 = ctx3 { Ctx.finalCheck = False, Ctx.lookedAt = [] }
      in doIterate ctx4
    else
      if not (Ctx.finalCheck ctx2) then -- Do a final run over all labels
        let ctx3 = ctx2 { Ctx.finalCheck = True }
            ctx4 = updateCursor ctx3
        in doIterate ctx4
      else
        if not (Ctx.seenAll ctx2) then -- In the final run, but not done yet
          let ctx3 = updateCursor ctx2
          in doIterate ctx3
        else
          ctx2 -- We've seen them all now

analyze :: Ast.Stmt -> Ctx
analyze stmt =
  let lbls = Utils.labelsOf stmt
      initialIns = Analysis.initializeIns lbls
      initialOuts = Analysis.initializeOuts lbls
      ctx = Ctx.initCtx stmt initialIns initialOuts
  in doIterate ctx
