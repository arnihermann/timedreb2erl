module Language.Erlang.Fold.TimedExp where

import Data.Generics

import Language.Fold
import Language.Erlang.Algebra
import Language.Erlang.Fold
import Language.Erlang.Fold.Identity
import Language.Erlang.Syntax


timedExpAlgebra = identityAlgebra {
    infixExpF = \iop exp0 exp -> case iop of
       OpMul -> updateExp iop exp0 exp
       OpDiv -> updateExp iop exp0 exp
       OpMod -> updateExp iop exp0 exp
       OpSub -> updateExp iop exp0 exp
       OpAdd -> updateExp iop exp0 exp
       _ -> InfixExp iop exp0 exp
}

updateExp iop exp0 exp = let e0time = includesTime exp0
                             etime = includesTime exp
                         in if e0time || etime
                            then InfixExp iop (addRt exp0 e0time) (addRt exp etime)
                            else InfixExp iop exp0 exp

includesTime (Apply (ModExp "rebeca" "now") []) = True
includesTime _ = False

addRt exp rt | not rt = Coercion (InfixExp OpMul rtfactor exp)
             | otherwise = exp
  where
    rtfactor = ExpVal $ AtomicLiteral "?RT_FACTOR"

{-includesTime = everything (||) (False `mkQ` timeExp)-}
  {-where-}
    {-timeExp (Apply (ModExp "rebeca" "now") []) = True-}
    {-timeExp _ = False-}

{-addRt exp rt | not rt = everywhere (mkT multRt) exp-}
             {-| otherwise = exp-}
  {-where-}
    {-multRt e@(ExpVal bv) = InfixExp OpMul rtfactor e-}
    {-multRt e@(ExpVar name) = InfixExp OpMul rtfactor e-}
    {-multRt exp = exp-}
    {-rtfactor = ExpVal $ AtomicLiteral "?RT_FACTOR"-}

fixTimedExp :: Program -> Program
fixTimedExp = fold timedExpAlgebra

