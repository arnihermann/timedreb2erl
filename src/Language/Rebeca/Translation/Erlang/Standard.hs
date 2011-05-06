module Language.Rebeca.Translation.Erlang.Standard where

import Prelude hiding (div, mod, const)

import Control.Applicative

import Language.Erlang.Syntax
import Language.Rebeca.Algebra
import qualified Language.Rebeca.Fold as F
import qualified Language.Rebeca.Absrebeca as R

-- TODO: add some kind of look environment (a state) to the equation

standardModel :: ModelAlgebra Name String String String [Name] [Name] [Function] Match Match Exp Function Program
standardModel = ModelAlgebra {
    model           = \envs rcs mai     -> Program (Module "test") [Export "none"] [Import "none"] (concat rcs ++ [mai])
  , envVar          = \tp               -> "env"
  , reactiveClass   = \id kr sv msi ms  -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
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
  , knownRebecs     = \tvds             -> tvds
  , stateVars       = \tvds             -> tvds
  , msgSrvInit      = \tps stms         -> Match (PatT $ (PatVal $ AtomicLiteral "initial"):(map PatVar tps)) Nothing (ap $ reverse stms)
  , msgSrv          = \id tps stms      -> Match (PatT $ (PatVal $ AtomicLiteral id):(map PatVar tps)) Nothing (ap $ reverse stms)
  , main            = \_                -> Function "main" [] (ExpVal $ AtomicLiteral "return main")
}

standardVal :: ValAlgebra String String String
standardVal = ValAlgebra {
    typedVarDecl    = \tvd -> case tvd of
                                R.TypedVarDecl typeName (R.Ident id) -> id
                                R.TypedVarDeclAss typeName (R.Ident id) exp -> id -- TODO, return Either String (String, String) ?
  , typedParameter  = \tp -> error "tp"
  , ident           = \id -> id    
}

standardStm :: StmAlgebra String Exp String (Maybe Exp) (Maybe Exp) Exp Exp Exp Exp
standardStm = StmAlgebra {
    ass         = \id exp               -> stm $ Apply "dict:store" [ExpVal $ AtomicLiteral id, exp, ExpVar "StateVars"]
  , local       = \tvd                  -> stm $ (ExpVal $ AtomicLiteral "return this")
  , call        = \id0 id exps aft dea  -> stm $ case aft of -- TODO lookup id0
                                                Nothing -> Seq (Apply "tr_send" [ExpVar id0, ExpVal $ AtomicLiteral id, ExpT exps]) retstm
                                                Just aft' -> Seq (Apply "tr_sendafter" [aft', ExpVar id0, ExpVal $ AtomicLiteral id, ExpT exps]) retstm
  , after       = id
  , deadline    = id
  , delay       = \exp                  -> stm $ Seq (Apply "tr_delay" [exp]) retstm
  , sel         = \exp cs elseifs els   -> stm $ If [Match (PatE exp) Nothing cs]
  , compStm     = \stms                 -> case stms of
                                                [] -> retstm
                                                [stm] -> Call stm params
                                                _ -> ap $ reverse stms
}

params = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]
stm = FunAnon [PatT [PatVar "StateVars", PatVar "LocalVars"]]
ap = foldr Call params
retstm = ExpT [ExpVar "StateVars", ExpVar "LocalVars"]

standardExp :: ExpAlgebra String Exp
standardExp = ExpAlgebra {
    lor         = \exp0 exp -> ExpVal $ AtomicLiteral "lor"
  , land        = \exp0 exp -> ExpVal $ AtomicLiteral "land"
  , bitor       = \exp0 exp -> ExpVal $ AtomicLiteral "bitor"
  , bitexor     = \exp0 exp -> ExpVal $ AtomicLiteral "bitexor"
  , bitand      = \exp0 exp -> ExpVal $ AtomicLiteral "bitand"
  , eq          = \exp0 exp -> InfixExp OpEq exp0 exp
  , neq         = \exp0 exp -> InfixExp OpNEq exp0 exp 
  , lthen       = \exp0 exp -> ExpVal $ AtomicLiteral "lthen"
  , grthen      = \exp0 exp -> ExpVal $ AtomicLiteral "grthen"
  , le          = \exp0 exp -> ExpVal $ AtomicLiteral "le"
  , ge          = \exp0 exp -> ExpVal $ AtomicLiteral "ge"
  , left        = \exp0 exp -> ExpVal $ AtomicLiteral "left"
  , right       = \exp0 exp -> ExpVal $ AtomicLiteral "right"
  , plus        = \exp0 exp -> ExpVal $ AtomicLiteral "plus"
  , minus       = \exp0 exp -> ExpVal $ AtomicLiteral "minus"
  , times       = \exp0 exp -> ExpVal $ AtomicLiteral "times"
  , div         = \exp0 exp -> ExpVal $ AtomicLiteral "div"
  , mod         = \exp0 exp -> ExpVal $ AtomicLiteral "mod"
  , expcoercion = \exp      -> ExpVal $ AtomicLiteral "expcoercion"
  , nondet      = \exps     -> ExpVal $ AtomicLiteral "nondet"
  , preop       = \op exp   -> Call (case op of
                                    R.Plus -> ExpVal $ AtomicLiteral "+"
                                    R.Negative -> ExpVal $ AtomicLiteral "-"
                                    R.Logicalneg -> ExpVal $ AtomicLiteral "not") exp
  , now         =              ExpVal $ AtomicLiteral "now"
  , const       = \con      -> ExpVal $ case con of
                                    R.Eint i -> NumberLiteral i
                                    R.Etrue -> AtomicLiteral "true"
                                    R.Efalse -> AtomicLiteral "false"
  , var         = \_        -> ExpVal $ AtomicLiteral "var" -- :: R.Exp -> exp
}



{-ds :: BooleanAlg b-}
   {--> NumberAlg n b-}
   {--> StateAlg Var n s-}
   {--> STrafoAlg (s -> s) (s -> b)-}
   {--> WhileAlg (s -> n) (s -> b) (s -> s)-}

ds :: ModelAlgebra id tvd tp env kr sv rc msi ms stm mai mod
    -> StmAlgebra id exp tvd aft dea cs eli el stm
    -> ValAlgebra id tvd tp
    -> ExpAlgebra id exp
    -> F.RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai 
ds mA sA vA eA = F.RebecaAlgebra {
    F.modelF            = \envs rcs mai     -> model mA envs rcs mai
  , F.envVarF           = \tp               -> envVar mA tp
  , F.reactiveClassF    = \id kr sv msi ms  -> reactiveClass mA id kr sv msi ms
  , F.knownRebecsF      = \tvds             -> knownRebecs mA tvds
  , F.stateVarsF        = \tvds             -> stateVars mA tvds
  , F.msgSrvInitF       = \tps stms         -> msgSrvInit mA tps stms
  , F.msgSrvF           = \id tps stms      -> msgSrv mA id tps stms
  , F.mainF             = \mai              -> main mA mai

  , F.typedVarDeclF     = \tvd  -> typedVarDecl vA tvd
  , F.typedParameterF   = \tp   -> typedParameter vA tp
  , F.identF            = \id   -> ident vA id

  , F.assF      = \id exp               -> ass sA id exp
  , F.localF    = \tvd                  -> local sA tvd
  , F.callF     = \id0 id expr aft dea  -> call sA id0 id expr aft dea
  , F.afterF    = \mexp                 -> after sA mexp
  , F.deadlineF = \mexp                 -> deadline sA mexp
  , F.delayF    = \exp                  -> delay sA exp
  , F.selF      = \exp cs elseifs els   -> sel sA exp cs elseifs els
  , F.compStmF  = \stms                 -> compStm sA stms

  , F.lorF          = \exp0 exp -> lor eA exp0 exp
  , F.landF         = \exp0 exp -> land eA exp0 exp
  , F.bitorF        = \exp0 exp -> bitor eA exp0 exp
  , F.bitexorF      = \exp0 exp -> bitexor eA exp0 exp
  , F.bitandF       = \exp0 exp -> bitand eA exp0 exp
  , F.eqF           = \exp0 exp -> eq eA exp0 exp
  , F.neqF          = \exp0 exp -> neq eA exp0 exp
  , F.lthenF        = \exp0 exp -> lthen eA exp0 exp
  , F.grthenF       = \exp0 exp -> grthen eA exp0 exp
  , F.leF           = \exp0 exp -> le eA exp0 exp
  , F.geF           = \exp0 exp -> ge eA exp0 exp
  , F.leftF         = \exp0 exp -> left eA exp0 exp
  , F.rightF        = \exp0 exp -> right eA exp0 exp
  , F.plusF         = \exp0 exp -> plus eA exp0 exp
  , F.minusF        = \exp0 exp -> minus eA exp0 exp
  , F.timesF        = \exp0 exp -> times eA exp0 exp
  , F.divF          = \exp0 exp -> div eA exp0 exp
  , F.modF          = \exp0 exp -> mod eA exp0 exp
  , F.expcoercionF  = \exp      -> expcoercion eA exp
  , F.nondetF       = \exps     -> nondet eA exps
  , F.preopF        = \aop exp  -> preop eA aop exp
  , F.nowF          =              now eA
  , F.constF        = \exp      -> const eA exp
  , F.varF          = \exp      -> var eA exp
}


rebecaAlgebra = ds standardModel standardStm standardVal standardExp

