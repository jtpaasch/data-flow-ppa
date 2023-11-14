module Examples where

import qualified Data.Set as Set
import qualified Ast as Ast

type Set = Set.Set


factorial :: Ast.Stmt
factorial =
  Ast.Seq
    (Ast.Assign 1 "y" (Ast.Avar "x"))
    (Ast.Seq
      (Ast.Assign 2 "z" (Ast.Anumb 1))
      (Ast.Seq
        (Ast.While 3 (Ast.BgtA (Ast.Avar "y") (Ast.Anumb 1))
          (Ast.Seq
            (Ast.Assign 4 "z" (Ast.Amult (Ast.Avar "z") (Ast.Avar "y")))
            (Ast.Assign 5 "y" (Ast.Aminus (Ast.Avar "y") (Ast.Anumb 1)))
          )    
        )
        (Ast.Assign 6 "y" (Ast.Anumb 0))
      )
    )

power :: Ast.Stmt
power =
  Ast.Seq
    (Ast.Assign 1 "z" (Ast.Anumb 1))
    (Ast.While 2 (Ast.BgtA (Ast.Avar "x") (Ast.Anumb 0))
      (Ast.Seq
        (Ast.Assign 3 "z" (Ast.Amult (Ast.Avar "z") (Ast.Avar "y")))
        (Ast.Assign 4 "x" (Ast.Aminus (Ast.Avar "x") (Ast.Anumb 1)))
      )
    )

test0 :: () -> Bool
test0 () =
  let expected = Set.fromList ["x", "y", "z"]
      result = Ast.freeVars power
  in result == expected

test1 :: () -> Bool
test1 () =
  let expected = 1
      result = Ast.initial power
  in result == expected

test2 :: () -> Bool
test2 () =
  let expected = Set.singleton 2 :: Ast.LabelSet
      result = Ast.final power
  in result == expected

test3 :: () -> Bool
test3 () =
  let expected = Set.fromList [1, 2, 3, 4] :: Ast.LabelSet
      result = Ast.labels power
  in result == expected

test4 :: () -> Bool
test4 () =
  let expected = Set.fromList [(1, 2), (2, 3), (3, 4), (4, 2)] :: Ast.FlowSet
      result = Ast.flow power
  in result == expected

test5 :: () -> Bool
test5 () =
  let expected = Set.fromList [(2, 1), (3, 2), (4, 3), (2, 4)] :: Ast.FlowSet
      result = Ast.flowR power
  in result == expected

test6 :: () -> Bool
test6 () = Ast.flowInvariant power

test7 :: () -> Bool
test7 () = Ast.flowRInvariant power

test8 :: () -> Bool
test8 () = Ast.hasIsolatedEntries power

test9 :: () -> Bool
test9 () = Ast.hasIsolatedExits power
