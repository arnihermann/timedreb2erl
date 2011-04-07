{-# LANGUAGE DeriveDataTypeable #-}
module Language.Erlang.Syntax where

import Data.Generics


type Name = String

type Arity = Integer

type Atom = String

{-type FunctionDecl = [FunctionClause]-}

data Program = Program Attribute [Attribute] [Attribute] [Function] -- module, export, import
  deriving (Eq,Ord,Show,Data,Typeable)

data Attribute = Module Name
               | Export Name
               | Import Name
  deriving (Eq,Ord,Show,Data,Typeable)

--type FunctionName = (Atom, Int)

{-data FunctionClause = FunctionClause Name [Exp] [ClauseGuard] [Exp]-}
  {-deriving (Eq,Ord,Show,Data,Typeable)-}

{-data FunctionBody = ClauseBody | RuleClauseBody-}
  {-deriving (Eq,Ord,Show,Data,Typeable)-}

{-data ClauseGuard = ClauseGuard Guard-}
  {-deriving (Eq,Ord,Show,Data,Typeable)-}

{-type Guard = [Exp]-}

{-type ClauseBody = [Exp]-}

{-data FunClause = FunClause [Exp] [ClauseGuard] ClauseBody-}
  {-deriving (Eq,Ord,Show,Data,Typeable)-}

data Function = Function Name [Pattern] Exp
  deriving (Eq,Ord,Show,Data,Typeable)

data BasicValue = AtomicLiteral String
    | NumberLiteral Integer
    | ProcessLiteral String
  deriving (Eq,Ord,Show,Data,Typeable)

data InfixOp = OpLT | OpLEq | OpGT | OpGEq | OpEq | OpNEq | OpLAnd | OpLOr 
    | OpMul | OpDiv | OpMod  | OpSub | OpBAnd | OpBXor | OpBOr | OpAdd
  deriving (Eq,Ord,Show,Data,Typeable)

data Exp = InfixExp InfixOp Exp Exp
    | Apply Name [Exp]
    | Case Exp [Match]
    | FunAnon [Pattern] Exp
    {-| FunAnon [FunClause]-}
    | Receive [Match]
    | If [Match]
    | Send Exp Exp
    | Seq Exp Exp
    | Assign Pattern Exp
    | ExpT [Exp] -- tuple
    | ExpL [Exp] -- list
    | ExpVal BasicValue -- value
    | ExpVar Name -- variable
  deriving (Eq,Ord,Show,Data,Typeable)

data Match = Match Pattern Guard Exp
  deriving (Eq,Ord,Show,Data,Typeable)

data Pattern = PatVar String
    | PatT [Pattern] -- tuple
    | PatL [Pattern] -- list
    | PatVal BasicValue
  deriving (Eq,Ord,Show,Data,Typeable)

data Guard = InfixGuard Guard Guard
    | GuardVal BasicValue
    | GuardVar Name 
    | GuardCall Guard [Guard]
    | GuardT [Guard] -- tuple
    | GuardL [Guard] -- list
  deriving (Eq,Ord,Show,Data,Typeable)

