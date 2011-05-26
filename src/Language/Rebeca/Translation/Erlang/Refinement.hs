module Language.Rebeca.Translation.Erlang.Refinement where

import Control.Monad.Reader
import Control.Monad.State
import Data.Either (either)
import Data.Maybe (fromMaybe)

import Language.Fold
import Language.Erlang.Builder
import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

type EnvVars = [(String, String)]
type KnownRebecs = [String]
type StateVars = [String]
type LocalVars = [String]

type CompilerConf = String -- the module name
type CompilerState = State (EnvVars, KnownRebecs, StateVars, LocalVars)

initialState = ([], [], [], [])

setEnvVars names = lift get >>= \(_, kr, sv, lv) -> put (names, kr, sv, lv)
setKnownRebecs names = lift get >>= \(env, _, sv, lv) -> put (env, names, sv, lv)
setStateVars names = lift get >>= \(env, kr, _, lv) -> put (env, kr, names, lv)
addLocalVar name = lift get >>= \(env, kr, sv, lv) -> put (env, kr, sv, name:lv)


getEnvVars = lift get >>= \(env, _, _, _) -> return env
getKnownRebecs = lift get >>= \(_, kr, _, _) -> return kr
getStateVars = lift get >>= \(_, _, sv, _) -> return sv
getLocalVars = lift get >>= \(_, _, _, lv) -> return lv

defaultVal "int" = "0"
defaultVal "time" = "0"
defaultVal "boolean" = "false"
defaultVal s = error ("no default value for " ++ s)

cast :: Exp -> String -> Exp
cast val tn | tn == "int" || tn == "time" = Apply (atomE "list_to_integer") [val]
            | otherwise = val

refinementAlgebra = RebecaAlgebra {
    identF = \id -> return id

  , modelF = \envs rcs mai -> do
        envs' <- sequence envs
        setEnvVars envs'
        rcs' <- sequence rcs
        mai' <- mai
        moduleName <- ask
        return (Program (Module moduleName) [Export ["main/1"]] [] [] (concat rcs' ++ [mai']))

  , envVarF = \tp -> tp

  , reactiveClassF = \id _ kr sv msi ms -> do
        id' <- id
        kr' <- kr
        sv' <- sv
        msi' <- msi
        ms' <- sequence ms
        let initialsv = Assign (varP "StateVars") (Apply (moduleE "dict" "from_list") [listE (map (\(_, i, d) -> tupleE [atomE i, either atomE (\x -> x) d]) sv')])
            initiallv = Assign (varP "LocalVars") (Apply (moduleE "dict" "new") [])
            recurs = Apply (atomE id') [varE "Env", varE "InstanceName", varE "KnownRebecs", varE "NewStateVars"]
        return ([ Function id' [varP "Env", varP "InstanceName"] $
              Receive [ Match (tupleP (map varP kr')) Nothing $
                Apply (atomE id') [ varE "Env", varE "InstanceName"
                                  , Apply (moduleE "dict" "from_list") [listE (map (\k -> tupleE [atomE k, varE k]) kr')]
                                  ]]
            , Function id' [varP "Env", varP "InstanceName", varP "KnownRebecs"] $
                Seq (Seq initialsv (Seq initiallv (Assign (tupleP [varP "NewStateVars", varP "_"]) (Receive [msi'])))) recurs
            , Function id' [varP "Env", varP "InstanceName", varP "KnownRebecs", varP "StateVars"] $
                Seq (Seq initiallv (Assign (tupleP [varP "NewStateVars", varP "_"]) (Receive ms'))) recurs
            ])
  , noKnownRebecsF = return []
  , knownRebecsF = \tvds -> do
        tvds' <- sequence tvds
        let ids = map (\(_, id, _) -> id) tvds'
        setKnownRebecs ids
        return ids

  , noStateVarsF = return []
  , stateVarsF = \tvds -> do
        tvds' <- sequence tvds
        let ids = map (\(_, id, _) -> id) tvds'
        setStateVars ids 
        return tvds'

  , msgSrvInitF = \tps stms -> do
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = tupleP [tupleP [varP "Sender", varP "TT", varP "DL"], atomP "initial", tupleP (map (varP . snd) tps')]
            pred = InfixExp OpLOr (InfixExp OpEq (varE "DL") (atomE "inf")) (InfixExp OpLEq (Apply (moduleE "rebeca" "now") []) (varE "DL"))
        return (Match patterns Nothing (Case pred [ Match (atomP "true") Nothing (formatReceive "initial" $ apply $ reverse stms')
                                                  , Match (atomP "false") Nothing (formatDrop "initial" retstm)]))

  , msgSrvF = \id tps stms -> do
        id' <- id
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = tupleP [tupleP [varP "Sender", varP "TT", varP "DL"], atomP id', tupleP (map (varP . snd) tps')]
            pred = InfixExp OpLOr (InfixExp OpEq (varE "DL") (atomE "inf")) (InfixExp OpLEq (Apply (moduleE "rebeca" "now") []) (varE "DL"))
        return (Match patterns Nothing (Case pred [ Match (atomP "true") Nothing (formatReceive id' $ apply $ reverse stms')
                                                  , Match (atomP "false") Nothing (formatDrop id' retstm)]))

  , vDeclAssignF = \id _ -> id
  , vDeclF = \id -> id

  , typedVarDeclF = \tn id -> do
        tn' <- tn
        id' <- id
        return (tn', id', Left (defaultVal tn'))
  , typedVarDeclAssF = \tn id exp -> do
        tn' <- tn
        id' <- id
        exp' <- exp
        return (tn', id', Right exp')

  , typedParameterF = \tn id -> tn >>= \tn' -> id >>= \id' -> return (tn', id')

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
                | id' `elem` sv = tupleE [Apply (moduleE "dict" "store") [atomE id', exp', varE "StateVars"], varE "LocalVars"]
                | id' `elem` lv = tupleE [varE "StateVars", Apply (moduleE "dict" "store") [atomE id', exp', varE "LocalVars"]]
                | otherwise = error $ "unknown variable name " ++ id'
        return (stm assignment)
  , localF = \tvd -> do
        (_, i, d) <- tvd
        addLocalVar i
        return (stm $ tupleE [varE "StateVars", Apply (moduleE "dict" "store") [atomE i, either atomE id d, varE "LocalVars"]])
  , callF = \id0 id exps aft dea -> do
        id0' <- id0
        id' <- id
        exps' <- sequence exps
        aft' <- aft
        dea' <- dea
        kr <- getKnownRebecs
        let target
                | id0' == "self" = atomE "self()"
                | id0' `elem` kr = Apply (moduleE "dict" "fetch") [atomE id0', varE "KnownRebecs"]
                | otherwise = varE id0' -- TODO: this is unsafe, we need to store formal parameters of the method in state as well, and check
        let deadline = fromMaybe (atomE "inf") dea'
        return $ stm $ case aft' of -- TODO lookup id0
                    Nothing -> Seq (Apply (moduleE "rebeca" "send") [target, atomE id', tupleE exps', deadline]) retstm
                    Just aft'' -> Seq (Apply (moduleE "rebeca" "sendafter") [aft'', target, atomE id', tupleE exps', deadline]) retstm
  , delayF = \exp -> exp >>= \exp' -> return (stm $ Seq (Apply (moduleE "rebeca" "delay") [exp']) retstm)
  , selF = \exp cs elseifs els -> do
        exp' <- exp
        cs' <- cs
        elseifs' <- sequence elseifs
        els' <- els
        let true = Match (atomP "true") Nothing cs'
            false = foldr (\f x -> f x) els' elseifs'
        return (stm $ Case exp' [true, false])

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
        let fn :: Match -> Match
            fn m = Match (atomP "false") Nothing (Case exp' [Match (atomP "true") Nothing cs', m])
        return fn

  , emptyElseStmF = return (Match (atomP "false") Nothing retstm)
  , elseStmF = \cs -> cs >>= \cs' -> return (Match (atomP "false") Nothing cs')

  , lorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLOr exp0' exp')
  , landF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLAnd exp0' exp')
  , bitorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (atomE "bitor")
  , bitexorF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (atomE "bitexor")
  , bitandF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (atomE "bitand")
  , eqF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpEq exp0' exp')
  , neqF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpNEq exp0' exp')
  , lthenF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLT exp0' exp')
  , grthenF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpGT exp0' exp')
  , leF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpLEq exp0' exp')
  , geF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpGEq exp0' exp')
  , leftF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (atomE "left")
  , rightF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (atomE "right")
  , plusF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpAdd exp0' exp')
  , minusF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpSub exp0' exp')
  , timesF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpMul exp0' exp')
  , divF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpDiv exp0' exp')
  , modF = \exp0 exp -> exp0 >>= \exp0' -> exp >>= \exp' -> return (InfixExp OpMod exp0' exp')
  , expcoercionF = \exp -> exp >>= \exp' -> return (atomE "expcoercion")
  , nondetF = \exps -> sequence exps >>= \exps' -> return (Apply (moduleE "rebeca" "nondet") [listE exps'])
  , preopF = \uop exp -> uop >>= \uop' -> exp >>= \exp' -> return (Call (ExpVal uop') exp')
  , nowF = return (Apply (moduleE "rebeca" "now") [])
  , constF = \con -> con >>= \con' -> return (ExpVal con')
  , varF = \ids -> do
        ids' <- sequence ids
        sv <- getStateVars
        kr <- getKnownRebecs
        lv <- getLocalVars
        env <- getEnvVars
        case ids' of
            "self":[id] -> return (Apply (moduleE "dict" "fetch") [atomE id, varE "StateVars"])
            [id] -> if id `elem` sv
                    then return (Apply (moduleE "dict" "fetch") [atomE id, varE "StateVars"])
                    else if id `elem` kr
                         then return (Apply (moduleE "dict" "fetch") [atomE id, varE "KnownRebecs"])
                         else if id `elem` lv
                              then return (Apply (moduleE "dict" "fetch") [atomE id, varE "LocalVars"])
                              else if id `elem` (map snd env)
                                   then return (Apply (moduleE "dict" "fetch") [atomE id, varE "Env"])
                                   else return (varE id) -- TODO: not safe, needs to lookup from formal parameters of method
            _ -> error "no variable or functionality not implemented"

  , constantIntF = \i -> return (num i)
  , constantTrueF = return (atom "true")
  , constantFalseF = return (atom "false")

  , unaryPlusF = return (atom "+")
  , unaryNegativeF = return (atom "-")
  , unaryComplementF = error "alg: unaryComplementF"
  , unaryLogicalNegF = return (atom "not")

  , opAssignF = return "="
  , opAssignMulF = error "alg: opAssignMulF"
  , opAssignDivF = error "alg: opAssignDivF"
  , opAssignModF = error "alg: opAssignModF"
  , opAssignAddF = error "alg: opAssignAddF"
  , opAssignSubF = error "alg: opAssignSubF"

  , mainF = \ins -> do
        ins' <- sequence ins
        envs <- getEnvVars
        let env = Assign (varP "Env") (Apply (moduleE "dict" "from_list") [listE $ map (\e -> tupleE [atomE (snd e), cast (varE (snd e)) (fst e)]) envs])
            spawns = foldr1 Seq (map fst ins')
            links = foldr1 Seq (map (fst . snd) ins')
            initials = foldr1 Seq (map (snd . snd) ins')
        return (Function "main" [listP $ (map (varP . snd) envs)] (Seq env (Seq spawns (Seq links initials))))

  , instanceDeclF = \tvd vds exps -> do
        tvd' <- tvd
        vds' <- sequence vds
        exps' <- sequence exps
        kr <- getKnownRebecs
        let (tn, id, de) = tvd'
            rebecName = id 
            fn = FunAnon [] (Apply (atomE tn) [varE "Env", Apply (atomE "list_to_atom") [stringE (rebecName)]]) -- TODO: rebecName is wrong
            spawn = Assign (varP (rebecName)) (Call (atomE "spawn") fn)
            link = Send (varE (rebecName)) (tupleE (map varE vds'))
            initial = Apply (moduleE "rebeca" "send") [varE (rebecName), atomE "initial"]
        return (spawn,(link,initial))
}

formatReceive msgsrv e = Seq (Apply (moduleE "io" "format") [stringE $ "~s." ++ msgsrv ++ "~n", listE [varE "InstanceName"]]) e
formatDrop msgsrv e = Seq (Apply (moduleE "io" "format") [stringE $ "dropping ~s." ++ msgsrv ++ "~n", listE [varE "InstanceName"]]) e
params = tupleE [varE "StateVars", varE "LocalVars"]
stm = FunAnon [tupleP [varP "StateVars", varP "LocalVars"]]
apply = foldr Call params
retstm = tupleE [varE "StateVars", varE "LocalVars"]

runRefine :: R.Model -> ReaderT CompilerConf CompilerState Program
runRefine model = fold refinementAlgebra model

translateRefinement :: String -> R.Model -> Program
translateRefinement modelName model = evalState (runReaderT (runRefine model) modelName) initialState


