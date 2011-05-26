{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.Erlang.Fold where

import Language.Fold
import Language.Erlang.Algebra
import Language.Erlang.Syntax


instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Program pr where
    fold f (Program mod exs ims des fns) = programF f (fold f mod) (map (fold f) exs) (map (fold f) ims) (map (fold f) des) (map (fold f) fns)

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Attribute at where
    fold f (Module name) = moduleF f name
    fold f (Export names) = exportF f names
    fold f (Import name) = importF f name
    fold f (Define name bv) = defineF f name (fold f bv) 

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Function fn where
    fold f (Function name pats exp) = functionF f name (map (fold f) pats) (fold f exp)

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) BasicValue bv where
    fold f (AtomicLiteral s) = atomicLiteralF f s
    fold f (StringLiteral s) = stringLiteralF f s
    fold f (NumberLiteral i) = numberLiteralF f i
    fold f (ProcessLiteral s) = processLiteralF f s

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) InfixOp iop where
    fold f OpLT = opLTF f
    fold f OpLEq = opLEqF f
    fold f OpGT = opGTF f
    fold f OpGEq = opGEqF f
    fold f OpEq = opEqF f
    fold f OpNEq = opNEqF f
    fold f OpLAnd = opLAndF f
    fold f OpLOr  = opLOrF f
    fold f OpMul = opMulF f
    fold f OpDiv = opDivF f
    fold f OpMod = opModF f
    fold f OpSub = opSubF f
    fold f OpBAnd = opBAndF f
    fold f OpBXor = opBXorF f
    fold f OpBOr = opBOrF f
    fold f OpAdd = opAddF f

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Exp exp where
    fold f (InfixExp iop exp0 exp) = infixExpF f (fold f iop) (fold f exp0) (fold f exp)
    fold f (ModExp n0 n) = modExpF f n0 n
    fold f (Apply exp exps) = applyF f (fold f exp) (map (fold f) exps)
    fold f (Call exp0 exp) = callF f (fold f exp0) (fold f exp)
    fold f (Case exp mats) = caseF f (fold f exp) (map (fold f) mats)
    fold f (FunAnon pats exp) = funAnonF f (map (fold f) pats) (fold f exp)
    fold f (Receive mats) = receiveF f (map (fold f) mats)
    fold f (If mats) = ifF f (map (fold f) mats)
    fold f (Send exp0 exp) = sendF f (fold f exp0) (fold f exp)
    fold f (Seq exp0 exp) = seqF f (fold f exp0) (fold f exp)
    fold f (Assign pat exp) = assignF f (fold f pat) (fold f exp)
    fold f (ExpT exps) = expTF f (map (fold f) exps)
    fold f (ExpL exps) = expLF f (map (fold f) exps)
    fold f (ExpVal bv) = expValF f (fold f bv)
    fold f (ExpVar name) = expVarF f name
    fold f (RecordCreate name attrs) = recordCreateF f name (map (\(k,v) -> (k, fold f v)) attrs)
    fold f (Coercion exp) = coercionF f (fold f exp)

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Match mat where
    fold f (Match pat gua exp) = matchF f (fold f pat) (fmap (fold f) gua) (fold f exp)

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Pattern pat where
    fold f (PatVar name) = patVarF f name
    fold f (PatT pats) = patTF f (map (fold f) pats)
    fold f (PatL pats) = patLF f (map (fold f) pats)
    fold f (PatVal bv) = patValF f (fold f bv)

instance Fold (ErlangAlgebra pr at fn bv iop exp mat pat gua) Guard gua where
    fold f (GuardVal bv) = guardValF f (fold f bv)
    fold f (GuardVar name) = guardVarF f name
    fold f (GuardCall g gs) = guardCallF f (fold f g) (map (fold f) gs)
    fold f (GuardT gs) = guardTF f (map (fold f) gs)
    fold f (GuardL gs) = guardLF f (map (fold f) gs)

