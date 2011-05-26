module Language.Erlang.Fold.Identity where

import Language.Fold
import Language.Erlang.Algebra
import Language.Erlang.Fold
import Language.Erlang.Syntax

identityAlgebra = ErlangAlgebra {
    programF = Program

  , moduleF = Module
  , exportF = Export
  , importF = Import
  , defineF = Define

  , functionF = Function

  , atomicLiteralF = AtomicLiteral
  , stringLiteralF = StringLiteral
  , numberLiteralF = NumberLiteral
  , processLiteralF = ProcessLiteral

  , opLTF = OpLT
  , opLEqF = OpLEq
  , opGTF = OpGT
  , opGEqF = OpGEq
  , opEqF = OpEq
  , opNEqF = OpNEq
  , opLAndF = OpLAnd
  , opLOrF = OpLOr
  , opMulF = OpMul
  , opDivF = OpDiv
  , opModF = OpMod
  , opSubF = OpSub
  , opBAndF = OpBAnd
  , opBXorF = OpBXor
  , opBOrF = OpBOr
  , opAddF = OpAdd

  , infixExpF = InfixExp
  , modExpF = ModExp
  , applyF = Apply
  , callF = Call
  , caseF = Case
  , funAnonF = FunAnon
  , receiveF = Receive
  , ifF = If
  , sendF = Send
  , seqF = Seq
  , assignF = Assign
  , expTF = ExpT
  , expLF = ExpL
  , expValF = ExpVal
  , expVarF = ExpVar
  , recordCreateF = RecordCreate

  , matchF = Match

  , patVarF = PatVar
  , patTF = PatT
  , patLF = PatL
  , patValF = PatVal

  , guardValF = GuardVal
  , guardVarF = GuardVar
  , guardCallF = GuardCall
  , guardTF = GuardT
  , guardLF = GuardL
}

translateIdentity :: Program -> Program
translateIdentity = fold identityAlgebra
