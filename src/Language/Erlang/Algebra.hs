module Language.Erlang.Builder where

import Language.Erlang.Syntax

data ErlangAlgebra
    pr -- 1. result for program
    at -- 2. result for attribute
    fn -- 3. result for function
    bv -- 4. result for basic value
    iop -- 5. result for infix op
    exp -- 6. result for expressions
    mat -- 7. result for match
    pat -- 8. result for patterns
    gua -- 9. result for guards
    = ErlangAlgebra {
    programF :: at -> [at] -> [at] -> [at] -> [fn] -> pr

  , moduleF :: String -> at
  , exportF :: [String] -> at
  , importF :: String -> at
  , defineF :: String -> bn -> at

  , functionF :: String -> [pat] -> exp -> fn

  , atomicLiteralF :: String -> bv
  , stringLiteralF :: String -> bf
  , numberLiteralF :: Integer -> bv
  , processLiteralF :: String -> bv

  , opLT :: iop
  , opLEq :: iop
  , opGT :: iop
  , opGEq :: iop
  , opEq :: iop
  , opNEq :: iop
  , opLAnd :: iop
  , opLOr :: iop
  , opMul :: iop
  , opDiv :: iop
  , opMod :: iop
  , opSub :: iop
  , opBAnd :: iop
  , opBXor :: iop
  , opBOr :: iop
  , opAdd :: iop

  , infixExpF :: iop -> exp -> exp -> exp
  , modExpF :: String -> String -> exp
  , applyF :: exp -> [exp] -> exp
  , callF :: exp -> exp -> exp
  , caseF :: exp -> [mat] -> exp
  , funAnonF :: [pat] -> exp -> exp
  , receiveF :: [mat] -> exp
  , ifF :: [mat] -> exp
  , sendF :: exp -> exp -> exp
  , seqF :: exp -> exp -> exp
  , assignF :: pat -> exp -> exp
  , expTF :: [exp] -> exp
  , expLF :: [exp] -> exp
  , expValF :: bv -> exp
  , expVarF :: String -> exp
  , recordCreateF :: String -> [(String, exp)] -> exp

  , matchF :: pat -> Maybe gua -> exp -> mat

  , patVarF :: String -> pat
  , patTF :: [pat] -> pat
  , patLF :: [pat] -> pat
  , patValF :: bv -> pat

  , guardValF :: bv -> gua
  , guardVarF :: String -> gua
  , guardCallF :: gua -> [gua] -> gua
  , guardTF :: [gua] -> gua
  , guardLF :: [gua] -> gua
}
