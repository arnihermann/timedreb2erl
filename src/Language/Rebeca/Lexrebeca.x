-- -*- haskell -*-
-- This Alex file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
module Language.Rebeca.Lexrebeca where



}


$l = [a-zA-Z\192 - \255] # [\215 \247]    -- isolatin1 letter FIXME
$c = [A-Z\192-\221] # [\215]    -- capital isolatin1 letter FIXME
$s = [a-z\222-\255] # [\247]    -- small isolatin1 letter FIXME
$d = [0-9]                -- digit
$i = [$l $d _ ']          -- identifier character
$u = [\0-\255]          -- universal: any character

@rsyms =    -- symbols and non-identifier-like reserved words
   \; | \( | \) | \{ | \} | \= | \, | \. | "else" \  "if" | \| \| | \& \& | \| | \^ | \& | \= \= | \! \= | \< | \> | \< \= | \> \= | \< \< | \> \> | \+ | \- | \* | \/ | \% | \? | \~ | \! | \* \= | \/ \= | \% \= | \+ \= | \- \= | \:

:-
"//" [.]* ; -- Toss single line comments
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ; 

$white+ ;
@rsyms { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }

$l $i*   { tok (\p s -> PT p (eitherResIdent (TV . share) s)) }


$d+      { tok (\p s -> PT p (TI $ share s))    }


{

tok f p s = f p s

share :: String -> String
share = id

data Tok =
   TS !String !Int    -- reserved words and symbols
 | TL !String         -- string literals
 | TI !String         -- integer literals
 | TV !String         -- identifiers
 | TD !String         -- double precision float literals
 | TC !String         -- character literals

 deriving (Eq,Show,Ord)

data Token = 
   PT  Posn Tok
 | Err Posn
  deriving (Eq,Show,Ord)

tokenPos (PT (Pn _ l _) _ :_) = "line " ++ show l
tokenPos (Err (Pn _ l _) :_) = "line " ++ show l
tokenPos _ = "end of file"

posLineCol (Pn _ l c) = (l,c)
mkPosToken t@(PT p _) = (posLineCol p, prToken t)

prToken t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s


data BTree = N | B String Tok BTree BTree deriving (Show)

eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

resWords = b ">>" 28 (b "-" 14 (b "(" 7 (b "%=" 4 (b "!=" 2 (b "!" 1 N N) (b "%" 3 N N)) (b "&&" 6 (b "&" 5 N N) N)) (b "+" 11 (b "*" 9 (b ")" 8 N N) (b "*=" 10 N N)) (b "," 13 (b "+=" 12 N N) N))) (b "<" 21 (b "/=" 18 (b "." 16 (b "-=" 15 N N) (b "/" 17 N N)) (b ";" 20 (b ":" 19 N N) N)) (b "==" 25 (b "<=" 23 (b "<<" 22 N N) (b "=" 24 N N)) (b ">=" 27 (b ">" 26 N N) N)))) (b "knownrebecs" 42 (b "else" 35 (b "boolean" 32 (b "^" 30 (b "?" 29 N N) (b "after" 31 N N)) (b "delay" 34 (b "deadline" 33 N N) N)) (b "if" 39 (b "env" 37 (b "else if" 36 N N) (b "false" 38 N N)) (b "int" 41 (b "initial" 40 N N) N))) (b "true" 49 (b "reactiveclass" 46 (b "msgsrv" 44 (b "main" 43 N N) (b "now" 45 N N)) (b "time" 48 (b "statevars" 47 N N) N)) (b "||" 52 (b "|" 51 (b "{" 50 N N) N) (b "~" 54 (b "}" 53 N N) N))))
   where b s n = let bs = id s
                  in B bs (TS bs n)

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, str) =
               case alexScan inp 0 of
                AlexEOF                -> []
                AlexError (pos, _, _)  -> [Err pos]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : (go inp')

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p, _, s) =
  case  s of
    []  -> Nothing
    (c:s) ->
             let p' = alexMove p c
              in p' `seq` Just (c, (p', c, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, s) = c
}
