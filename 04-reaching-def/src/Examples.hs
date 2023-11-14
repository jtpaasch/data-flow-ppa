module Examples where

import qualified Ast as Ast

-- From p. 37 of Nielson, Nielson, and Hankin.
-- Computes the x-th power of the number stored in y.
-- The result goes in z.
power :: Ast.Stmt
power =
  Ast.StmtSeq
    (Ast.StmtAssign
      (Ast.Label 1)
      (Ast.Var "z")
      (Ast.AexpNumb (Ast.Numb 1))
    )
    (Ast.StmtWhile
      (Ast.Label 2)
      (Ast.BexpLtA
        (Ast.AexpNumb (Ast.Numb 0))
        (Ast.AexpVar (Ast.Var "x"))
      )
      (Ast.StmtSeq
        (Ast.StmtAssign
          (Ast.Label 3)
          (Ast.Var "z")
          (Ast.AexpMult
            (Ast.AexpVar (Ast.Var "z"))
            (Ast.AexpVar (Ast.Var "y"))
          )
        )
        (Ast.StmtAssign
          (Ast.Label 4)
          (Ast.Var "x")
          (Ast.AexpMinus
            (Ast.AexpVar (Ast.Var "x"))
            (Ast.AexpNumb (Ast.Numb 1))
          )
        )
      )
    )

-- This program is bad because it is not label-consistent
-- (the label 3 occurs twice).
powerBad :: Ast.Stmt
powerBad =
  Ast.StmtSeq
    (Ast.StmtAssign
      (Ast.Label 1)
      (Ast.Var "z")
      (Ast.AexpNumb (Ast.Numb 1))
    )
    (Ast.StmtWhile
      (Ast.Label 2)
      (Ast.BexpLtA
        (Ast.AexpNumb (Ast.Numb 0))
        (Ast.AexpVar (Ast.Var "x"))
      )
      (Ast.StmtSeq
        (Ast.StmtAssign
          (Ast.Label 3)
          (Ast.Var "z")
          (Ast.AexpMult
            (Ast.AexpVar (Ast.Var "z"))
            (Ast.AexpVar (Ast.Var "y"))
          )
        )
        (Ast.StmtAssign
          (Ast.Label 3)
          (Ast.Var "x")
          (Ast.AexpMinus
            (Ast.AexpVar (Ast.Var "x"))
            (Ast.AexpNumb (Ast.Numb 1))
          )
        )
      )
    )

-- From p. 43 of Nielson, Nielson, and Hankin.
prog1 :: Ast.Stmt
prog1 =
  Ast.StmtSeq
    (Ast.StmtAssign
      (Ast.Label 1)
      (Ast.Var "x")
      (Ast.AexpNumb (Ast.Numb 5))
    )
    (Ast.StmtSeq
      (Ast.StmtAssign
        (Ast.Label 2)
        (Ast.Var "y")
        (Ast.AexpNumb (Ast.Numb 1))
      )
      (Ast.StmtWhile
        (Ast.Label 3)
        (Ast.BexpLtA
          (Ast.AexpNumb (Ast.Numb 1))
          (Ast.AexpVar (Ast.Var "x"))
        )
        (Ast.StmtSeq
          (Ast.StmtAssign
            (Ast.Label 4)
            (Ast.Var "y")
            (Ast.AexpMult
              (Ast.AexpVar (Ast.Var "x"))
              (Ast.AexpVar (Ast.Var "y"))
            )
          )
          (Ast.StmtAssign
            (Ast.Label 5)
            (Ast.Var "x")
            (Ast.AexpMinus
              (Ast.AexpVar (Ast.Var "x"))
              (Ast.AexpNumb (Ast.Numb 1))
            )
          )
        )
      )
    )
