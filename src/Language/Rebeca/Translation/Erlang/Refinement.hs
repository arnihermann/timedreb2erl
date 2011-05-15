module Language.Rebeca.Translation.Erlang.Refinement where

import Control.Monad.State
import Data.Maybe (fromMaybe)

import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

type EnvVars = [String]
type KnownRebecs = [String]
type StateVars = [String]
type LocalVars = [String]

type CompilerState = State (EnvVars, KnownRebecs, StateVars, LocalVars)

initialState = ([], [], [], [])

setEnvVars names = get >>= \(_, kr, sv, lv) -> put (names, kr, sv, lv)
setKnownRebecs names = get >>= \(env, _, sv, lv) -> put (env, names, sv, lv)
setStateVars names = get >>= \(env, kr, _, lv) -> put (env, kr, names, lv)
{-setLocalVars names = get >>= \(env, kr, sv, _) -> put (env, kr, sv, names)-}
addLocalVar name = get >>= \(env, kr, sv, lv) -> put (env, kr, sv, name:lv)


getEnvVars = State $ \s@(env, kr, sv, lv) -> (env, s)
getKnownRebecs = State $ \s@(env, kr, sv, lv) -> (kr, s)
getStateVars = State $ \s@(env, kr, sv, lv) -> (sv, s)
getLocalVars = State $ \s@(env, kr, sv, lv) -> (lv, s)

{-resetState :: CompilerState ()-}
{-resetState = State $ \_ -> ((), initialState)-}

defaultVal "int" = "0"
defaultVal "time" = "0"
defaultVal "boolean" = "false"
defaultVal s = error ("no default value for " ++ s)

refinementAlgebra = RebecaAlgebra {
    identF = \id -> return id

  , modelF = \envs rcs mai -> do
        envs' <- sequence envs
        rcs' <- sequence rcs
        mai' <- mai
        return (Program (Module "test") [Export "none"] [Import "none"] (concat rcs' ++ [mai']))

  , envVarF = \tp -> tp

  , reactiveClassF = \id _ kr sv msi ms -> do
        id' <- id
        kr' <- kr
        sv' <- sv
        msi' <- msi
        ms' <- sequence ms
        let initialsv = Assign (PatVar "StateVars") (Apply "dict:from_list" [ExpL (concat $ map (\(d, i) -> [ExpVal $ AtomicLiteral i, ExpVal $ AtomicLiteral d]) sv')])
            initiallv = Assign (PatVar "LocalVars") (Apply "dict:from_list" [ExpL []])
        return ([ Function id' [PatVar "Env", PatVar "InstanceName"] $
            Receive [ Match (PatT (map PatVar kr')) Nothing $
                        Apply id' [ ExpVar "Env", ExpVar "InstanceName"
                                 , Apply "dict:from_list" [ExpL (concat $ map (\k -> [ExpVal $ AtomicLiteral k, ExpVar k]) kr')]
                                 ]
                    ]
            , Function id' [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs"] $
                Seq initialsv
                    (Seq initiallv
                         (Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive [msi'])))
            , Function id' [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs", PatVar "StateVars"] $
                Seq initiallv
                    (Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive ms'))
            ])
  , noKnownRebecsF = return []
  , knownRebecsF = \tvds -> do
        tvds' <- sequence tvds
        setKnownRebecs (map snd tvds')
        return (map snd tvds')

  , noStateVarsF = return []
  , stateVarsF = \tvds -> do
        tvds' <- sequence tvds
        setStateVars (map snd tvds')
        return tvds'

  , msgSrvInitF = \tps stms -> do
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = PatT [PatT [PatVar "Sender", PatVar "TT", PatVar "DL"], PatVal $ AtomicLiteral "initial", PatT (map PatVar tps')]
            now = Assign (PatVar "TimeNow") (Apply "tr_now" [])
            pred = InfixExp OpLAnd (InfixExp OpEq (ExpVar "DL") (ExpVal $ AtomicLiteral "inf")) (InfixExp OpLT (ExpVar "TimeNow") (ExpVar "DL"))
        return (Match patterns Nothing (Seq now (If [Match (PatE pred) Nothing (apply $ reverse stms')])))

  , msgSrvF = \id tps stms -> do
        id' <- id
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = PatT [PatT [PatVar "Sender", PatVar "TT", PatVar "DL"], PatVal $ AtomicLiteral id', PatT (map PatVar tps')]
            now = Assign (PatVar "TimeNow") (Apply "tr_now" [])
            pred = InfixExp OpLAnd (InfixExp OpEq (ExpVar "DL") (ExpVal $ AtomicLiteral "inf")) (InfixExp OpLT (ExpVar "TimeNow") (ExpVar "DL"))            
        return (Match patterns Nothing (Seq now (If [Match (PatE pred) Nothing (apply $ reverse stms')])))

  , vDeclAssignF = \id _ -> id
  , vDeclF = \id -> id

  , typedVarDeclF = \tn id -> do
        tn' <- tn
        id' <- id
        return (defaultVal tn', id')
  , typedVarDeclAssF = \tn id _ -> do
        tn' <- tn
        id' <- id
        return (defaultVal tn', id')

  , typedParameterF = \_ id -> id

  , basicTypeIntF = return "int"
  , basicTypeTimeF = return "time"
  , basicTypeBooleanF = return "boolean"

  , builtInF = \bt -> bt
  , classTypeF = \id -> id

  , assF = \id aop exp -> do
        id' <- id
        aop' <- aop
        exp' <- exp
        sv <- getStateVars
        lv <- getLocalVars
        let assignment
                | id' `elem` sv = ExpT [Apply "dict:store" [ExpVal $ AtomicLiteral id', exp', ExpVar "StateVars"], ExpVar "LocalVars"]
                | id' `elem` lv = ExpT [ExpVar "StateVars", Apply "dict:store" [ExpVal $ AtomicLiteral id', exp', ExpVar "LocalVars"]]
                | otherwise = error $ "unknown variable name " ++ id'
        return (stm assignment)
  , localF = \tvd -> do
        (d, i) <- tvd
        addLocalVar i
        return (stm $ ExpT [ExpVar "StateVars", Apply "dict:store" [ExpVal $ AtomicLiteral i, ExpVal $ AtomicLiteral d, ExpVar "LocalVars"]])
  , callF = \id0 id exps aft dea -> do
        id0' <- id0
        id' <- id
        exps' <- sequence exps
        aft' <- aft
        dea' <- dea
        kr <- getKnownRebecs
        let target
                | id0' == "self" = ExpVal $ AtomicLiteral "self()"
                | id0' `elem` kr = Apply "dict:fetch" [ExpVal $ AtomicLiteral id0', ExpVar "KnownRebecs"]
                | otherwise = ExpVar id0' -- TODO: this is unsafe, we need to store formal parameters of the method in state as well, and check
        let deadline = fromMaybe (ExpVal $ AtomicLiteral "inf") dea'
        return $ stm $ case aft' of -- TODO lookup id0
                    Nothing -> Seq (Apply "tr_send" [target, ExpVal $ AtomicLiteral id', ExpT exps', deadline]) retstm
                    Just aft'' -> Seq (Apply "tr_sendafter" [aft'', target, ExpVal $ AtomicLiteral id', ExpT exps', deadline]) retstm
  , delayF = \exp -> exp >>= \exp' -> return (stm $ Seq (Apply "tr_delay" [exp']) retstm)
  , selF = \exp cs elseifs els -> do
        exp' <- exp
        cs' <- cs
        elseifs' <- sequence elseifs
        els' <- els
        return (stm $ If [Match (PatE exp') Nothing cs'])

  , singleCompStmF = \stm -> stm >>= \stm' -> return (Call stm' params)
  , multCompStmF = \stms -> sequence stms >>= \stms' ->
        return $ case stms' of
            [] -> retstm
            [stm] -> Call stm params
            _ -> apply $ reverse stms'

  , noAfterF = return Nothing
  , withAfterF = \exp -> exp >>= \exp' -> return (Just exp')

  , noDeadlineF = return Nothing
  , withDeadlineF = \exp -> exp >>= \exp' -> return (Just exp')

  , elseifStmF = \exp cs -> do
        exp' <- exp
        cs' <- cs
        return (Match (PatVal $ AtomicLiteral "true") Nothing retstm)

  , emptyElseStmF = return (Match (PatVal $ AtomicLiteral "true") Nothing retstm)
  , elseStmF = \cs -> cs >>= \cs' -> return (Match (PatVal $ AtomicLiteral "true") Nothing retstm)

  , lorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLOr exp0' exp')
  , landF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLAnd exp0' exp')
  , bitorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "bitor")
  , bitexorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "bitexor")
  , bitandF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "bitand")
  , eqF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpEq exp0' exp')
  , neqF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpNEq exp0' exp')
  , lthenF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "lthen")
  , grthenF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "grthen")
  , leF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "le")
  , geF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "ge")
  , leftF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "left")
  , rightF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "right")
  , plusF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpAdd exp0' exp')
  , minusF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "minus")
  , timesF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "times")
  , divF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "div")
  , modF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "mod")
  , expcoercionF = \exp -> exp >>= \exp' -> return (ExpVal $ AtomicLiteral "expcoercion")
  , nondetF = \exps -> sequence exps >>= \exps' -> return (ExpVal $ AtomicLiteral "nondet")
  , preopF = \uop exp -> uop >>= \uop' -> exp >>= \exp' -> return (Call (ExpVal uop') exp')
  , nowF = return (Apply "tr_now" [])
  , constF = \con -> con >>= \con' -> return (ExpVal con')
  , varF = \ids -> do
        ids' <- sequence ids
        sv <- getStateVars
        kr <- getKnownRebecs
        lv <- getLocalVars
        env <- getEnvVars
        case ids' of
            "self":[id] -> return (Apply "dict:fetch" [ExpVal $ AtomicLiteral id, ExpVar "StateVars"])
            [id] -> if id `elem` sv
                    then return (Apply "dict:fetch" [ExpVal $ AtomicLiteral id, ExpVar "StateVars"])
                    else if id `elem` kr
                         then return (Apply "dict:fetch" [ExpVal $ AtomicLiteral id, ExpVar "KnownRebecs"])
                         else if id `elem` lv
                              then return (Apply "dict:fetch" [ExpVal $ AtomicLiteral id, ExpVar "LocalVars"])
                              else if id `elem` sv
                                   then return (Apply "dict:fetch" [ExpVal $ AtomicLiteral id, ExpVar "Env"])
                                   else return (ExpVar id) -- TODO: not safe, needs to lookup from formal parameters of method
            _ -> error "no variable or functionality not implemented"

  , constantIntF = \i -> return (NumberLiteral i)
  , constantTrueF = return (AtomicLiteral "true")
  , constantFalseF = return (AtomicLiteral "false")

  , unaryPlusF = return (AtomicLiteral "+")
  , unaryNegativeF = return (AtomicLiteral "-")
  , unaryComplementF = error "alg: unaryComplementF"
  , unaryLogicalNegF = return (AtomicLiteral "not")

  , opAssignF = return "="
  , opAssignMulF = error "alg: opAssignMulF"
  , opAssignDivF = error "alg: opAssignDivF"
  , opAssignModF = error "alg: opAssignModF"
  , opAssignAddF = error "alg: opAssignAddF"
  , opAssignSubF = error "alg: opAssignSubF"

  , mainF = \ins -> sequence ins >>= \ins' -> return (Function "main" [] (ExpVal $ AtomicLiteral "return main"))

  , instanceDeclF = \tvd vds exps -> do
        tvd' <- tvd
        vds' <- sequence vds
        exps' <- sequence exps
        return (ExpVal $ AtomicLiteral "return instancedecl")
}

params = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]
stm = FunAnon [PatT [PatVar "StateVars", PatVar "LocalVars"]]
apply = foldr Call params
retstm = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]

run :: R.Model -> CompilerState Program
run model = fold refinementAlgebra model

translateRefinement :: R.Model -> Program
translateRefinement model = evalState (run model) initialState

