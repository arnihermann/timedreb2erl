module Language.Rebeca.Translation.Erlang.Refinement where

import Control.Monad.State

import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Fold
import Language.Rebeca.FoldM

type EnvVars = [String]
type KnownRebecs = [String]
type StateVars = [String]
type LocalVars = [String]

type CompilerState = State (EnvVars, KnownRebecs, StateVars, LocalVars)

initialState = ([], [], [], [])

{-refinementAlgebra :: RebecaAlgebra String Program String [Function] [Name] [Name] Match Match vd Name Name bt tn Exp Exp (Maybe Exp) (Maybe Exp) eli el Exp BasicValue BasicValue aop Function ins-}
refinementAlgebra = RebecaAlgebra {
    identF = \id -> id

  , modelF = \envs rcs mai -> Program (Module "test") [Export "none"] [Import "none"] (concat rcs ++ [mai])

  , envVarF = \tp -> "env"

  , reactiveClassF = \id _ kr sv msi ms -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
                                                Receive [ Match (PatT (map PatVar kr)) Nothing $
                                                            Apply id [ ExpVar "Env", ExpVar "InstanceName"
                                                                     , Apply "dict:from_list" (concat $ map (\k -> [ExpVal $ AtomicLiteral k, ExpVar k]) kr)
                                                                     ]
                                                        ]
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive [msi])
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs", PatVar "StateVars"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive ms)
                                           ]
  , noKnownRebecsF = []
  , knownRebecsF = id 

  , noStateVarsF = []
  , stateVarsF = id

  , msgSrvInitF = \tps stms -> Match (PatT $ (PatVal $ AtomicLiteral "initial"):(map PatVar tps)) Nothing (apply $ reverse stms)

  , msgSrvF = \id tps stms -> Match (PatT $ (PatVal $ AtomicLiteral id):(map PatVar tps)) Nothing (apply $ reverse stms)

  , vDeclAssignF = \id exp -> error "alg: vDeclAssignF"
  , vDeclF = \id -> error "alg: vDeclF"

  , typedVarDeclF = \tn id -> id
  , typedVarDeclAssF = \tn id exp -> id

  , typedParameterF = \tn id -> id

  , basicTypeIntF = error "alg: basicTypeIntF"
  , basicTypeTimeF = error "alg: basicTypeTimeF"
  , basicTypeBooleanF = error "alg: basicTypeBooleanF"

  , builtInF = \bt -> error "alg: builtInF"
  , classTypeF = \id -> error "alg: classTypeF"

  , assF = \id aop exp -> stm $ Apply "dict:store" [ExpVal $ AtomicLiteral id, exp, ExpVar "StateVars"]
  , localF = \tvd -> stm $ (ExpVal $ AtomicLiteral "return this")
  , callF = \id0 id exps aft dea -> stm $ case aft of -- TODO lookup id0
                                                Nothing -> Seq (Apply "tr_send" [ExpVar id0, ExpVal $ AtomicLiteral id, ExpT exps]) retstm
                                                Just aft' -> Seq (Apply "tr_sendafter" [aft', ExpVar id0, ExpVal $ AtomicLiteral id, ExpT exps]) retstm
  , delayF = \exp -> stm $ Seq (Apply "tr_delay" [exp]) retstm
  , selF = \exp cs elseifs els -> stm $ If [Match (PatE exp) Nothing cs]

  , singleCompStmF = \stm -> Call stm params
  , multCompStmF = \stms -> case stms of
                            [] -> retstm
                            [stm] -> Call stm params
                            _ -> apply $ reverse stms

  , noAfterF = Nothing
  , withAfterF = \exp -> Just exp

  , noDeadlineF = Nothing
  , withDeadlineF = \exp -> Just exp

  , elseifStmF = \exp cs -> error "alg: elseifStmF"

  , emptyElseStmF = error "alg: emptyElseStmF"
  , elseStmF = \cs -> error "alg: elseStmF"

  , lorF = \exp0 exp -> ExpVal $ AtomicLiteral "lor"
  , landF = \exp0 exp -> ExpVal $ AtomicLiteral "land"
  , bitorF = \exp0 exp -> ExpVal $ AtomicLiteral "bitor"
  , bitexorF = \exp0 exp -> ExpVal $ AtomicLiteral "bitexor"
  , bitandF = \exp0 exp -> ExpVal $ AtomicLiteral "bitand"
  , eqF = \exp0 exp -> InfixExp OpEq exp0 exp
  , neqF = \exp0 exp -> InfixExp OpNEq exp0 exp
  , lthenF = \exp0 exp -> ExpVal $ AtomicLiteral "lthen"
  , grthenF = \exp0 exp -> ExpVal $ AtomicLiteral "grthen"
  , leF = \exp0 exp -> ExpVal $ AtomicLiteral "le"
  , geF = \exp0 exp -> ExpVal $ AtomicLiteral "ge"
  , leftF = \exp0 exp -> ExpVal $ AtomicLiteral "left"
  , rightF = \exp0 exp -> ExpVal $ AtomicLiteral "right"
  , plusF = \exp0 exp -> InfixExp OpAdd exp0 exp
  , minusF = \exp0 exp -> ExpVal $ AtomicLiteral "minus"
  , timesF = \exp0 exp -> ExpVal $ AtomicLiteral "times"
  , divF = \exp0 exp -> ExpVal $ AtomicLiteral "div"
  , modF = \exp0 exp -> ExpVal $ AtomicLiteral "mod"
  , expcoercionF = \exp -> ExpVal $ AtomicLiteral "expcoercion"
  , nondetF = \exps -> ExpVal $ AtomicLiteral "nondet"
  , preopF = \uop exp -> Call (ExpVal uop) exp
  , nowF = ExpVal $ AtomicLiteral "now"
  , constF = \con -> ExpVal con
  , varF = \_ -> ExpVal $ AtomicLiteral "var" -- :: R.Exp -> exp

  , constantIntF = \i -> NumberLiteral i
  , constantTrueF = AtomicLiteral "true"
  , constantFalseF = AtomicLiteral "false"

  , unaryPlusF = AtomicLiteral "+"
  , unaryNegativeF = AtomicLiteral "-"
  , unaryComplementF = error "alg: unaryComplementF"
  , unaryLogicalNegF = AtomicLiteral "not"

  , opAssignF = error "alg: opAssignF"
  , opAssignMulF = error "alg: opAssignMulF"
  , opAssignDivF = error "alg: opAssignDivF"
  , opAssignModF = error "alg: opAssignModF"
  , opAssignAddF = error "alg: opAssignAddF"
  , opAssignSubF = error "alg: opAssignSubF"

  , mainF = \_ -> Function "main" [] (ExpVal $ AtomicLiteral "return main")

  , instanceDeclF = \tvd vds exps -> error "alg: instanceDeclF"
}

params = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]
stm = FunAnon [PatT [PatVar "StateVars", PatVar "LocalVars"]]
apply = foldr Call params
retstm = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]

{-translateRefinment :: R.Model -> CompilerState Program-}
{-translateRefinment mod = evalState (fold refinementAlgebra mod) initialState -}

translateRefinment :: R.Model -> Program
translateRefinment = fold refinementAlgebra

