-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Language.Rebeca.Parrebeca where
import Language.Rebeca.Absrebeca
import Language.Rebeca.Lexrebeca
import Language.Rebeca.ErrM

}

%name pModel Model

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '!' { PT _ (TS _ 1) }
 '!=' { PT _ (TS _ 2) }
 '%' { PT _ (TS _ 3) }
 '%=' { PT _ (TS _ 4) }
 '&' { PT _ (TS _ 5) }
 '&&' { PT _ (TS _ 6) }
 '(' { PT _ (TS _ 7) }
 ')' { PT _ (TS _ 8) }
 '*' { PT _ (TS _ 9) }
 '*=' { PT _ (TS _ 10) }
 '+' { PT _ (TS _ 11) }
 '+=' { PT _ (TS _ 12) }
 ',' { PT _ (TS _ 13) }
 '-' { PT _ (TS _ 14) }
 '-=' { PT _ (TS _ 15) }
 '.' { PT _ (TS _ 16) }
 '/' { PT _ (TS _ 17) }
 '/=' { PT _ (TS _ 18) }
 ':' { PT _ (TS _ 19) }
 ';' { PT _ (TS _ 20) }
 '<' { PT _ (TS _ 21) }
 '<<' { PT _ (TS _ 22) }
 '<=' { PT _ (TS _ 23) }
 '=' { PT _ (TS _ 24) }
 '==' { PT _ (TS _ 25) }
 '>' { PT _ (TS _ 26) }
 '>=' { PT _ (TS _ 27) }
 '>>' { PT _ (TS _ 28) }
 '?' { PT _ (TS _ 29) }
 '^' { PT _ (TS _ 30) }
 'after' { PT _ (TS _ 31) }
 'boolean' { PT _ (TS _ 32) }
 'deadline' { PT _ (TS _ 33) }
 'delay' { PT _ (TS _ 34) }
 'else' { PT _ (TS _ 35) }
 'else if' { PT _ (TS _ 36) }
 'env' { PT _ (TS _ 37) }
 'false' { PT _ (TS _ 38) }
 'if' { PT _ (TS _ 39) }
 'initial' { PT _ (TS _ 40) }
 'int' { PT _ (TS _ 41) }
 'knownrebecs' { PT _ (TS _ 42) }
 'main' { PT _ (TS _ 43) }
 'msgsrv' { PT _ (TS _ 44) }
 'now' { PT _ (TS _ 45) }
 'reactiveclass' { PT _ (TS _ 46) }
 'statevars' { PT _ (TS _ 47) }
 'time' { PT _ (TS _ 48) }
 'true' { PT _ (TS _ 49) }
 '{' { PT _ (TS _ 50) }
 '|' { PT _ (TS _ 51) }
 '||' { PT _ (TS _ 52) }
 '}' { PT _ (TS _ 53) }
 '~' { PT _ (TS _ 54) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Model :: { Model }
Model : ListEnvVar ListReactiveClass Main { Model (reverse $1) (reverse $2) $3 } 


EnvVar :: { EnvVar }
EnvVar : 'env' TypedParameter ';' { EnvVar $2 } 


ListEnvVar :: { [EnvVar] }
ListEnvVar : {- empty -} { [] } 
  | ListEnvVar EnvVar { flip (:) $1 $2 }


ReactiveClass :: { ReactiveClass }
ReactiveClass : 'reactiveclass' Ident '(' Integer ')' '{' KnownRebecs StateVars MsgSrvInit ListMsgSrv '}' { ReactiveClass $2 $4 $7 $8 $9 (reverse $10) } 


ListReactiveClass :: { [ReactiveClass] }
ListReactiveClass : {- empty -} { [] } 
  | ListReactiveClass ReactiveClass { flip (:) $1 $2 }


KnownRebecs :: { KnownRebecs }
KnownRebecs : {- empty -} { NoKnownRebecs } 
  | 'knownrebecs' '{' ListTypedVarDecl '}' { KnownRebecs $3 }


StateVars :: { StateVars }
StateVars : {- empty -} { NoStateVars } 
  | 'statevars' '{' ListTypedVarDecl '}' { StateVars $3 }


MsgSrvInit :: { MsgSrvInit }
MsgSrvInit : 'msgsrv' 'initial' '(' ListTypedParameter ')' '{' ListStm '}' { MsgSrvInit $4 (reverse $7) } 


MsgSrv :: { MsgSrv }
MsgSrv : 'msgsrv' Ident '(' ListTypedParameter ')' '{' ListStm '}' { MsgSrv $2 $4 (reverse $7) } 


ListMsgSrv :: { [MsgSrv] }
ListMsgSrv : {- empty -} { [] } 
  | ListMsgSrv MsgSrv { flip (:) $1 $2 }


VarDecl :: { VarDecl }
VarDecl : Ident '=' Exp { VDeclAssign $1 $3 } 
  | Ident { VDecl $1 }


ListVarDecl :: { [VarDecl] }
ListVarDecl : VarDecl { (:[]) $1 } 
  | VarDecl ',' ListVarDecl { (:) $1 $3 }


TypedVarDecl :: { TypedVarDecl }
TypedVarDecl : TypeName Ident { TypedVarDecl $1 $2 } 
  | TypeName Ident '=' Exp { TypedVarDeclAss $1 $2 $4 }


ListTypedVarDecl :: { [TypedVarDecl] }
ListTypedVarDecl : {- empty -} { [] } 
  | TypedVarDecl { (:[]) $1 }
  | TypedVarDecl ';' ListTypedVarDecl { (:) $1 $3 }


TypedParameter :: { TypedParameter }
TypedParameter : TypeName Ident { TypedParameter $1 $2 } 


ListTypedParameter :: { [TypedParameter] }
ListTypedParameter : {- empty -} { [] } 
  | TypedParameter { (:[]) $1 }
  | TypedParameter ',' ListTypedParameter { (:) $1 $3 }


BasicType :: { BasicType }
BasicType : 'int' { Tint } 
  | 'time' { Ttime }
  | 'boolean' { Tboolean }


TypeName :: { TypeName }
TypeName : BasicType { BuiltIn $1 } 
  | Ident { ClassType $1 }


Stm :: { Stm }
Stm : Stm ';' { $1 } 
  | Ident AssignmentOp Exp ';' { Ass $1 $2 $3 }
  | TypedVarDecl ';' { Local $1 }
  | Ident '.' Ident '(' ListExp ')' After Deadline ';' { Call $1 $3 $5 $7 $8 }
  | 'delay' '(' Exp ')' ';' { Delay $3 }
  | 'if' '(' Exp ')' CompStm ListElseifStm ElseStm { Sel $3 $5 (reverse $6) $7 }


ListStm :: { [Stm] }
ListStm : {- empty -} { [] } 
  | ListStm Stm { flip (:) $1 $2 }


CompStm :: { CompStm }
CompStm : Stm { SingleCompoundStm $1 } 
  | '{' ListStm '}' { MultCompoundStm (reverse $2) }


After :: { After }
After : {- empty -} { NoAfter } 
  | 'after' '(' Exp ')' { WithAfter $3 }


Deadline :: { Deadline }
Deadline : {- empty -} { NoDeadline } 
  | 'deadline' '(' Exp ')' { WithDeadline $3 }


ElseifStm :: { ElseifStm }
ElseifStm : 'else if' '(' Exp ')' CompStm { ElseifStm $3 $5 } 


ListElseifStm :: { [ElseifStm] }
ListElseifStm : {- empty -} { [] } 
  | ListElseifStm ElseifStm { flip (:) $1 $2 }


ElseStm :: { ElseStm }
ElseStm : {- empty -} { EmptyElseStm } 
  | 'else' CompStm { ElseStm $2 }


ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } 
  | Ident '.' ListIdent { (:) $1 $3 }


Exp :: { Exp }
Exp : Exp '||' Exp2 { Elor $1 $3 } 
  | Exp1 { $1 }


Exp2 :: { Exp }
Exp2 : Exp2 '&&' Exp3 { Eland $1 $3 } 
  | Exp3 { $1 }


Exp3 :: { Exp }
Exp3 : Exp3 '|' Exp4 { Ebitor $1 $3 } 
  | Exp4 { $1 }


Exp4 :: { Exp }
Exp4 : Exp4 '^' Exp5 { Ebitexor $1 $3 } 
  | Exp5 { $1 }


Exp5 :: { Exp }
Exp5 : Exp5 '&' Exp6 { Ebitand $1 $3 } 
  | Exp6 { $1 }


Exp6 :: { Exp }
Exp6 : Exp6 '==' Exp7 { Eeq $1 $3 } 
  | Exp6 '!=' Exp7 { Eneq $1 $3 }
  | Exp7 { $1 }


Exp7 :: { Exp }
Exp7 : Exp7 '<' Exp8 { Elthen $1 $3 } 
  | Exp7 '>' Exp8 { Egrthen $1 $3 }
  | Exp7 '<=' Exp8 { Ele $1 $3 }
  | Exp7 '>=' Exp8 { Ege $1 $3 }
  | Exp8 { $1 }


Exp8 :: { Exp }
Exp8 : Exp8 '<<' Exp9 { Eleft $1 $3 } 
  | Exp8 '>>' Exp9 { Eright $1 $3 }
  | Exp9 { $1 }


Exp9 :: { Exp }
Exp9 : Exp9 '+' Exp10 { Eplus $1 $3 } 
  | Exp9 '-' Exp10 { Eminus $1 $3 }
  | Exp10 { $1 }


Exp10 :: { Exp }
Exp10 : Exp10 '*' Exp11 { Etimes $1 $3 } 
  | Exp10 '/' Exp11 { Ediv $1 $3 }
  | Exp10 '%' Exp11 { Emod $1 $3 }
  | Exp11 { $1 }


Exp11 :: { Exp }
Exp11 : '(' Exp ')' { Eexpcoercion $2 } 
  | '?' '(' ListExp ')' { ENondet $3 }
  | Exp12 { $1 }


Exp12 :: { Exp }
Exp12 : UnaryOperator Exp11 { Epreop $1 $2 } 
  | Exp13 { $1 }


Exp13 :: { Exp }
Exp13 : 'now' '(' ')' { Enow } 
  | Constant { Econst $1 }
  | Exp14 { $1 }


Exp14 :: { Exp }
Exp14 : ListIdent { Evar $1 } 
  | '(' Exp ')' { $2 }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }


Exp1 :: { Exp }
Exp1 : Exp2 { $1 } 


Constant :: { Constant }
Constant : Integer { Eint $1 } 
  | 'true' { Etrue }
  | 'false' { Efalse }


ListConstant :: { [Constant] }
ListConstant : {- empty -} { [] } 
  | Constant { (:[]) $1 }
  | Constant ',' ListConstant { (:) $1 $3 }


UnaryOperator :: { UnaryOperator }
UnaryOperator : '+' { Plus } 
  | '-' { Negative }
  | '~' { Complement }
  | '!' { Logicalneg }


AssignmentOp :: { AssignmentOp }
AssignmentOp : '=' { Assign } 
  | '*=' { AssignMul }
  | '/=' { AssignDiv }
  | '%=' { AssignMod }
  | '+=' { AssignAdd }
  | '-=' { AssignSub }


Main :: { Main }
Main : 'main' '{' ListInstanceDecl '}' { Main $3 } 


InstanceDecl :: { InstanceDecl }
InstanceDecl : TypedVarDecl '(' ListVarDecl ')' ':' '(' ListExp ')' { InstanceDecl $1 $3 $7 } 


ListInstanceDecl :: { [InstanceDecl] }
ListInstanceDecl : {- empty -} { [] } 
  | InstanceDecl { (:[]) $1 }
  | InstanceDecl ';' ListInstanceDecl { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

