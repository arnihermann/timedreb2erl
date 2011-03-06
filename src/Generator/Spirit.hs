module Generator.Spirit (transModel) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Char (toLower, toUpper)
import Data.Generics
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.StringTemplate

import TemplateHelper
import Generator.Types

import Language.Rebeca.Absrebeca
import Language.Rebeca.ErrM


initConf = RebecConfiguration
    { reactiveClassName = ""
    , knownRebecNames = []
    , knownRebecRecordName = Nothing
    , localVarNames = []
    , stateVarRecordName = Nothing
    , stateVarNames = []
    , localVarRecordName = Nothing
    , records = []
    }

firstUpper "" = ""
firstUpper (c:cs) = toUpper c : cs

firstLower "" = ""
firstLower (c:cs) = toLower c : cs

identifers (TypedVarDecl _ (Ident name)) = [name]
identifers (TypedVarDeclAss _ (Ident name) _) = [name]

typedIdentifiers (TypedVarDecl typename (Ident name)) = [(defaultValue typename, name)]
typedIdentifiers (TypedVarDeclAss typename (Ident name) _) = [(defaultValue typename, name)]

defaultValue (BuiltIn Tint) = "0"
defaultValue (BuiltIn Ttime) = "0"
defaultValue (BuiltIn Tboolean) = "false"

withT f = withTemplate $ "spirit/" ++ f

transModel :: FilePath -> Model -> Result
transModel filename (Model environmentvars reactiveclasses main) =
    let moduleTpl = withT "module.erl"
        monitorTpl = withT "monitor.erl"
        runTpl = withT "run.erl"
        moduleName = head $ splitOn "." filename
        rcState = map (flip runState initConf . transReactiveClass) reactiveclasses
        reactiveClasses = map fst rcState
        recs = intercalate "\n" $ concatMap (records . snd) rcState
        mainCode = evalState (transMain main) initConf
    in (render $ ( setAttribute "moduleName" moduleName
                 . setAttribute "reactiveClasses" reactiveClasses
                 . setAttribute "main" mainCode
                 ) moduleTpl,
                 recs,
                 render $ setAttribute "moduleName" moduleName monitorTpl,
                 render $ setAttribute "moduleName" moduleName runTpl)

createRecord :: String -> [String] -> String
createRecord recname fields = "-record(" ++ recname ++ ", {" ++ intercalate ", " fields ++ "})."

setKnownRebecState :: KnownRebecs -> String -> TransState [(String, String)]
setKnownRebecState knownrebecs reactiveclassname = do
    st <- get
    put st { knownRebecNames = knownrebecnames }
    st <- get
    when (knownrebecnames /= []) (put st { knownRebecRecordName = Just ('#' : recname), records = createRecord recname (map firstLower knownrebecnames) : records st })
    return $ map (firstLower &&& firstUpper) knownrebecnames
  where 
    knownrebecnames = everything (++) ([] `mkQ` identifers) knownrebecs
    recname = reactiveclassname ++ "_knownrebecs"

setStateVarState :: StateVars -> String -> TransState [(String, String)]
setStateVarState statevars reactiveclassname = do
    st <- get
    put st { stateVarNames = statevarnames }
    st <- get
    when (statevarnames /= []) (put st { stateVarRecordName = Just ('#' : recname), records = createRecordWithValues recname defaultvarvalues : records st })
    return $ map (\s -> (firstLower $ fst s, snd s)) defaultvarvalues
  where
    statevarnames = everything (++) ([] `mkQ` identifers) statevars
    defaultvarvalues = everything (++) ([] `mkQ` typedIdentifiers) statevars
    recname = reactiveclassname ++ "_statevars"
    createRecordWithValues recname fields = "-record(" ++ recname ++ ", {" ++ intercalate ", " (map (\f -> snd f ++ "=" ++ fst f) fields) ++ "})."

setLocalVarState :: MsgSrvInit -> [MsgSrv] -> String -> TransState ()
setLocalVarState initmsgsrv msgsrvs reactiveclassname = do
    st <- get
    put st { localVarNames = localvarnames }
    st <- get
    when (localvarnames /= []) (put st { localVarRecordName = Just ('#' : recname), records = createRecord recname (map firstLower localvarnames) : records st })
  where 
    localvarnames = everything (++) ([] `mkQ` identifers) initmsgsrv ++ everything (++) ([] `mkQ` identifers) msgsrvs
    recname = reactiveclassname ++ "_localvars"

transReactiveClass :: ReactiveClass -> TransState String
transReactiveClass (ReactiveClass (Ident id) n knownrebecs statevars initmsgsrv msgsrvs) = do
    knownrebecvalues <- setKnownRebecState knownrebecs reactiveclassname
    defaultvarvalues <- setStateVarState statevars reactiveclassname
    setLocalVarState initmsgsrv msgsrvs reactiveclassname
    st <- get
    put st { reactiveClassName = reactiveclassname }
    msgSrvInitStr <- transMsgSrvInit initmsgsrv
    st <- get
    let msgSrvs = map (flip evalState st . transMsgSrv) msgsrvs
    return $ render $
        ( setAttribute "reactiveClassName" reactiveclassname
        . setAttribute "knownRebecRecordName" (knownRebecRecordName st)
        . setAttribute "knownRebecNames" (map firstUpper (knownRebecNames st))
        . setAttribute "knownRebecValues" knownrebecvalues
        . setAttribute "stateVarRecordName" (stateVarRecordName st)
        . setAttribute "localVarRecordName" (localVarRecordName st)
        . setAttribute "msgSrvInit" msgSrvInitStr
        . setAttribute "msgSrvs" msgSrvs
        ) classTpl
  where
    classTpl = withT "reactiveclass.erl"
    reactiveclassname = firstLower id

transMsgSrvInit :: MsgSrvInit -> TransState String
transMsgSrvInit (MsgSrvInit typedparameters stms) = transMsgSrvG "initial" typedparameters stms

transMsgSrv :: MsgSrv -> TransState String
transMsgSrv (MsgSrv (Ident id) typedparameters stms) = transMsgSrvG (firstLower id) typedparameters stms

transMsgSrvG :: String -> [TypedParameter] -> [Stm] -> TransState String
transMsgSrvG id typedparameters stms =  do
    st <- get
    let statements = nestStm "ProcTime" $ map (flip evalState st . transStm) stms
    return $ render $
        ( setAttribute "reactiveClassName" (reactiveClassName st)
        . setAttribute "msgSrvName" id
        . setAttribute "statements" statements
        . setAttribute "formals" formals
        ) msgSrvTpl
  where
    msgSrvTpl = withT "msgsrv.erl"
    idents (Ident name) = [firstUpper name]
    formals = everything (++) ([] `mkQ` idents) typedparameters

nestStm :: String -> [String] -> String
nestStm t [] = "{" ++ t ++ ", StateVars, LocalVars}"
nestStm t [stm] = stm ++ " ({" ++ t ++ ", StateVars, LocalVars})"
nestStm t stms = foldr wrap ("{" ++ t ++ ", StateVars, LocalVars}") (reverse stms)
  where
    wrap item acc = item ++ " (" ++ sep ++ acc ++ ")"
    sep = "\n                              "

transStm :: Stm -> TransState String
transStm stm = transStm' stm >>= \s -> return $ "fun({Time, StateVars, LocalVars}) -> " ++ s ++ " end"

transStm' :: Stm -> TransState String
transStm' (Ass (Ident id) assignmentop exp) = do
    e <- transExp exp
    st <- get
    let rec
            | id `elem` stateVarNames st = ("StateVars" ++ as (fromJust $ stateVarRecordName st) e, "LocalVars")
            | id `elem` localVarNames st = ("StateVars", "LocalVars" ++ as (fromJust $ localVarRecordName st) e)
            | otherwise = error $ "unknown variable name " ++ id
    return $ "{Time, " ++ fst rec ++ ", " ++ snd rec ++ "}"
  where as r e = r ++ "{" ++ firstLower id ++ "=" ++ e ++ "}"

transStm' (Local (TypedVarDecl _ id)) = return "{Time, StateVars, LocalVars}"

transStm' (Local (TypedVarDeclAss typename (Ident id) exp)) = do
    e <- transExp exp
    st <- get
    return $ "{Time, StateVars, LocalVars" ++ fromJust (localVarRecordName st) ++ "{" ++ firstLower id ++ "=" ++ e ++ "}}"

transStm' (Call (Ident id0) (Ident id) exps after deadline) = do
    st <- get
    let params = map (flip evalState st . transExp) exps
    let target = knownRebOrSelf id0 (fromJust $ knownRebecRecordName st)
    deadl <- transDeadline deadline
    case after of
        NoAfter -> return $ "tr_send(Time, " ++ target ++ ", " ++ firstLower id ++ ", " ++ "{" ++ intercalate "," params ++ "}, " ++ deadl ++ "), {Time, StateVars, LocalVars}"
        (WithAfter texp) -> do
            aft <- transExp texp
            return $ "tr_sendafter(Time + " ++ aft ++ ", " ++ target ++ ", " ++ firstLower id ++ ", " ++ "{" ++ intercalate "," params ++ "}, " ++ deadl ++ "), {Time, StateVars, LocalVars}"
  where
    transDeadline NoDeadline = return "inf"
    transDeadline (WithDeadline exp) = transExp exp
    knownRebOrSelf rebname recname =
        if rebname == "self"
        then "self()"
        else "KnownRebecs" ++ recname ++ "." ++ firstLower rebname

transStm' (Delay exp) = do
    del <- transExp exp
    return $ "{Time + " ++ del ++ ", StateVars, LocalVars}"

transStm' (Sel exp compstm elseifs elsestm) = do
    cond <- transExp exp
    comp <- transCompStm compstm
    elsestatement <- transElseStm elsestm
    st <- get
    let eifs = map (flip evalState st . transElseifStm) elseifs
    return $ render $
        ( setAttribute "exp" cond
        . setAttribute "statements" (comp ++ ";")
        . setAttribute "elseStm" elsestatement
        . setAttribute "elseIfs" eifs
        ) selTpl
  where
    selTpl = withT "sel.erl"
    transElseifStm (ElseifStm exp compstm) = do
        st <- get
        cond <- transExp exp
        comp <- transCompStm compstm
        return $ cond ++ " -> " ++ "\n" ++ comp ++ ";"
    transElseStm (ElseStm compstm) = transCompStm compstm
    transElseStm EmptyElseStm = return "{Time, StateVars, LocalVars}"

transCompStm :: CompStm -> TransState String
transCompStm x = case x of
    SingleCompoundStm stm -> transStm stm >>= \s -> return $ nestStm "Time" [s]
    MultCompoundStm stms -> do
        st <- get
        let statements = map (flip evalState st . transStm) stms
        return $ nestStm "Time" statements

transBinExp :: Exp -> Exp -> String -> TransState String
transBinExp exp0 exp op = do
    e0 <- transExp exp0
    e <- transExp exp
    return $ e0 ++ " " ++ op ++ " " ++ e

transExp :: Exp -> TransState String
transExp x = case x of
    Elor exp0 exp -> transBinExp exp0 exp "orelse"
    Eland exp0 exp -> transBinExp exp0 exp "andalso"
    Ebitor exp0 exp -> error "Ebitor not implemented"
    Ebitexor exp0 exp -> error "Ebitexor not implemented"
    Ebitand exp0 exp -> error "Ebitand not implemented"
    Eeq exp0 exp -> transBinExp exp0 exp "=:="
    Eneq exp0 exp -> transBinExp exp0 exp "=/="
    Elthen exp0 exp -> transBinExp exp0 exp "<"
    Egrthen exp0 exp -> transBinExp exp0 exp ">"
    Ele exp0 exp -> error "Ele not implemented"
    Ege exp0 exp -> error "Ege not implemented"
    Eleft exp0 exp -> error "Eleft not implemented"
    Eright exp0 exp -> error "Eright not implemented"
    Eplus exp0 exp -> transBinExp exp0 exp "+"
    Eminus exp0 exp -> transBinExp exp0 exp "-"
    Etimes exp0 exp -> transBinExp exp0 exp "*"
    Ediv exp0 exp -> transBinExp exp0 exp "div"
    Emod exp0 exp -> transBinExp exp0 exp "rem"
    Eexpcoercion exp -> transExp exp >>= \e -> return $ "(" ++ e ++ ")" -- TODO: never run!
    ENondet exps -> do
        st <- get
        let texps = map (flip evalState st . transExp) exps
        return $ "tr_nondet([" ++ intercalate "," texps ++ "])"
    Epreop unaryoperator exp ->
        let op = case unaryoperator of
                    Plus -> "+"
                    Negative -> "-"
                    Complement -> error "Complement not implemented"
                    Logicalneg -> "not "
        in transExp exp >>= \e -> return $ op ++ e
    Enow -> return "Time"
    Econst constant -> transConstant constant
    Evar (Ident "self":[Ident id]) -> do
        st <- get
        return $ "StateVars#" ++ fromJust (stateVarRecordName st) ++ "." ++ firstLower id
    Evar [Ident id] -> do
        st <- get
        return $ if id `elem` stateVarNames st
                 then "StateVars" ++ fromJust (stateVarRecordName st) ++ "." ++ firstLower id
                 else if id `elem` knownRebecNames st
                      then "KnownRebecs" ++ fromJust (knownRebecRecordName st) ++ "." ++ firstLower id
                      else if id `elem` localVarNames st
                           then "LocalVars" ++ fromJust (localVarRecordName st) ++ "." ++ firstLower id
                           else firstUpper id
    Evar ids -> error "Evar not implemented"

transConstant :: Constant -> TransState String
transConstant x = return $ case x of
    Eint n -> show n
    Etrue -> "true"
    Efalse -> "false"


transMain :: Main -> TransState String
transMain (Main instancedecls) = do
    st <- get
    let decl = intercalate sep (map (flip evalState st . transInstanceDecl) instancedecls)
        conf = intercalate sep (map (flip evalState st . transInstanceDeclConf) instancedecls)
        msg = intercalate sep (map (flip evalState st . transInstanceInitialMsg) instancedecls)
        stor = "  RebecTimes = ets:new('times', [public])"
        times = intercalate sep (map (flip evalState st . transInstanceTime) instancedecls)
        spirit = "Spirit = spawn(fun() -> spirit(RebecTimes) end)" ++ sep ++ "register(spirit, Spirit)"
    return $ intercalate sep [stor, spirit, decl, times, conf, msg]
  where sep = ",\n    "

transInstanceTime :: InstanceDecl -> TransState String
transInstanceTime (InstanceDecl typedvardecl vardecls constants) =
    let typename (ClassType (Ident id)) = [id]
        cls = head $ everything (++) ([] `mkQ` typename) typedvardecl
        inst = head $ everything (++) ([] `mkQ` identifers) typedvardecl
    in return $ "ets:insert(RebecTimes, {" ++ firstUpper inst ++", 0})"

transInstanceDecl :: InstanceDecl -> TransState String
transInstanceDecl (InstanceDecl typedvardecl vardecls constants) =
    let typename (ClassType (Ident id)) = [id]
        cls = head $ everything (++) ([] `mkQ` typename) typedvardecl
        inst = head $ everything (++) ([] `mkQ` identifers) typedvardecl
    in return $ firstUpper inst ++ " = spawn(fun() -> " ++ firstLower cls ++ "(RebecTimes, list_to_atom(\"" ++ firstLower inst ++"\")) end)"

transInstanceDeclConf :: InstanceDecl -> TransState String
transInstanceDeclConf (InstanceDecl typedvardecl vardecls constants) =
    let inst = head $ everything (++) ([] `mkQ` identifers) typedvardecl
        idents (Ident name) = [firstUpper name]
        vard = intercalate ", " (everything (++) ([] `mkQ` idents) vardecls)
    in return $ firstUpper inst ++ " ! " ++ "{" ++ vard ++ "}"

transInstanceInitialMsg :: InstanceDecl -> TransState String
transInstanceInitialMsg (InstanceDecl typedvardecl vardecls exps) = do
    st <- get
    let inst = head $ everything (++) ([] `mkQ` identifers) typedvardecl
        params = intercalate ", " (map (flip evalState st . transExp) exps)
    return $ "tr_send(0, " ++ firstUpper inst ++ ", initial, {" ++ params ++ "})"

