module Language.Rebeca.Translation.Erlang.Refinement where

import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Fold

refinementAlgebra = RebecaAlgebra {
    modelF = \envs rcs mai -> Program (Module "test") [Export "none"] [Import "none"] (concat rcs ++ [mai])
  , envVarF = \tp -> "env"
  , reactiveClassF = \id kr sv msi ms -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
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
  , knownRebecsF = \tvds -> tvds
  , stateVarsF = \tvds -> tvds
  , msgSrvInitF = \tps stms -> Match (PatT $ (PatVal $ AtomicLiteral "initial"):(map PatVar tps)) Nothing (ap $ reverse stms)
  , msgSrvF = \id tps stms -> Match (PatT $ (PatVal $ AtomicLiteral id):(map PatVar tps)) Nothing (ap $ reverse stms)
  , mainF = \_ -> Function "main" [] (ExpVal $ AtomicLiteral "return main")

  , typedVarDeclF = \tvd -> case tvd of
                                R.TypedVarDecl typeName (R.Ident id) -> id
                                R.TypedVarDeclAss typeName (R.Ident id) exp -> id -- TODO, return Either String (String, String) ?
  , typedParameterF = \(R.TypedParameter _ (R.Ident s)) -> s
  , identF = \id -> id

  , assF = \id exp -> stm $ Apply "dict:store" [ExpVal $ AtomicLiteral id, exp, ExpVar "StateVars"]
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
                            _ -> ap $ reverse stms

  , noAfterF = Nothing
  , withAfterF = \exp -> Just exp

  , noDeadlineF = Nothing
  , withDeadlineF = \exp -> Just exp

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
  , plusF = \exp0 exp -> ExpVal $ AtomicLiteral "plus"
  , minusF = \exp0 exp -> ExpVal $ AtomicLiteral "minus"
  , timesF = \exp0 exp -> ExpVal $ AtomicLiteral "times"
  , divF = \exp0 exp -> ExpVal $ AtomicLiteral "div"
  , modF = \exp0 exp -> ExpVal $ AtomicLiteral "mod"
  , expcoercionF = \exp -> ExpVal $ AtomicLiteral "expcoercion"
  , nondetF = \exps -> ExpVal $ AtomicLiteral "nondet"
  , preopF = \op exp -> Call (case op of
                                R.Plus -> ExpVal $ AtomicLiteral "+"
                                R.Negative -> ExpVal $ AtomicLiteral "-"
                                R.Logicalneg -> ExpVal $ AtomicLiteral "not") exp
  , nowF = ExpVal $ AtomicLiteral "now"
  , constF = \con -> ExpVal $ case con of
                        R.Eint i -> NumberLiteral i
                        R.Etrue -> AtomicLiteral "true"
                        R.Efalse -> AtomicLiteral "false"
  , varF = \_ -> ExpVal $ AtomicLiteral "var" -- :: R.Exp -> exp
}

params = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]
stm = FunAnon [PatT [PatVar "StateVars", PatVar "LocalVars"]]
ap = foldr Call params
retstm = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]

refine :: R.Model -> Program
refine = foldModel refinementAlgebra
