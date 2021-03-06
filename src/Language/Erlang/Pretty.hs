module Language.Erlang.Pretty where

import Data.Char (toLower, toUpper)
import Data.List (intercalate, intersperse, nub)
import Text.PrettyPrint

import Language.Erlang.Syntax


firstUpper "" = ""
firstUpper (c:cs) = toUpper c : cs

firstLower "" = ""
firstLower (c:cs) = toLower c : cs

emptyLine :: Doc
emptyLine = text ""

dot :: Doc
dot = text "."

inBlock :: [Doc] -> Doc
inBlock docs = nest 2 (vcat docs)

inBlock1 :: Doc -> Doc
inBlock1 = nest 2 

commaSep :: [Doc] -> Doc
commaSep docs = hcat (punctuate (comma <> space) docs)

commaSepV :: [Doc] -> Doc
commaSepV docs = vcat (punctuate (comma <> space) docs)

pipeSep :: [Doc] -> Doc
pipeSep docs = hcat (punctuate (space <> text "|" <> space) docs)

val (AtomicLiteral s) = text (firstLower s)
val (StringLiteral s) = doubleQuotes (text s)
val (NumberLiteral i) = text (show i)
val (ProcessLiteral s) = text (firstUpper s)

pattern (PatVar s) = text (firstUpper s)
pattern (PatT ps) = braces (commaSep $ map pattern ps)
pattern (PatL ps) = brackets (commaSep $ map pattern ps)
pattern (PatVal v) = val v

infop op = text $ case op of
    OpLT -> "<"
    OpLEq -> "=<"
    OpGT -> ">"
    OpGEq -> ">=" 
    OpEq -> "==" 
    OpNEq -> "=/=" 
    OpLAnd -> "andalso" 
    OpLOr -> "orelse" 
    OpMul -> "*" 
    OpDiv -> "/" 
    OpMod -> "rem" 
    OpSub -> "-" 
    OpBAnd -> "BAND" 
    OpBXor -> "BXOR" 
    OpBOr -> "BOR" 
    OpAdd -> "+"

match (Match pat guard exp) = pattern pat <+> text "->" $+$ inBlock1 (expr exp)

expr (InfixExp op exp0 exp) = expr exp0 <+> infop op <+> expr exp
expr (ModExp n1 n2) = text (firstLower n1) <> colon <> text (firstLower n2)
expr (Apply name exps) = expr name <> (parens $ commaSep (map expr exps))
expr (Call exp0 exp) = expr exp0 <> (parens $ expr exp)
expr (Case exp ms) = text "case" <+> expr exp <+> text "of"
    $+$ inBlock (punctuate semi (map match ms))
    $+$ text "end"
expr (FunAnon ps exp) = text "fun" <> (parens $ commaSep (map pattern ps)) <+> text "->"
    $+$ inBlock1 (expr exp)
    $+$ text "end"
expr (Receive ms) = text "receive"
    $+$ inBlock (punctuate semi (map match ms))
    $+$ text "end"
expr (If ms) = text "if"
    $+$ inBlock (punctuate semi (map match ms))
    $+$ text "end"
expr (Send exp0 exp) = expr exp0 <+> text "!" <+> expr exp
expr (Seq exp0 exp) = expr exp0 <> comma $+$ expr exp
expr (Assign pat exp) = pattern pat <+> text "=" <+> expr exp
expr (ExpT exps) = braces $ commaSep (map expr exps)
expr (ExpL exps) = brackets $ commaSep (map expr exps)
expr (ExpVal v) = val v
expr (ExpVar "self") = text "self" <> parens empty
expr (ExpVar name) = text $ firstUpper name
expr (RecordCreate name attrs) = text "#" <> text name <> braces (commaSepV $ map (\(k,v) -> text k <> text "=" <> expr v) attrs)
expr (Coercion exp) = parens (expr exp)

func (Function name formals exp) = text (firstLower name) <> (parens $ commaSep $ map pattern formals) <+> text "->" $+$ inBlock [expr exp]

attr (Module name) = text "-module" <> (parens $ text (firstLower name))
attr (Export names) = text "-export" <> (parens $ brackets $ commaSep (map text names))
attr (Import name) = text "-include" <> (parens $ doubleQuotes $ text name)
attr (Define name value) = text "-define" <> (parens $ text name <> comma <+> val value)

program (Program mod exp imp def fns) = vcat $ intersperse emptyLine [ attr mod <> dot
                                                                     , vcat (map (enddot . attr) exp)
                                                                     , vcat (map (enddot . attr) imp)
                                                                     , vcat (map (enddot . attr) def)
                                                                     , vcat (map (enddot . func) fns) ]
  where
    enddot = flip (<>) dot

renderProgram :: Program -> String
renderProgram = render . program
