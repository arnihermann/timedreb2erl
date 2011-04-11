module Language.Rebeca.Translation.Erlang.Standard where

import Prelude hiding (div, mod, const)

import Control.Applicative

import Language.Erlang.Syntax
import Language.Rebeca.Algebra
import qualified Language.Rebeca.Fold as F
import qualified Language.Rebeca.Absrebeca as R


standardModel :: ModelAlgebra Name Pattern Pattern Pattern Pattern Pattern [Function] Match Function Exp Function Program
standardModel = ModelAlgebra {
    model           = \envs rcs mai     -> Program (Module "test") [Export "none"] [Import "none"] (concat rcs ++ [mai])
  , envVar          = \tp               -> PatVar "env"
  , reactiveClass   = \id kr sv msi ms  -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
                                                Receive [ Match kr Nothing $
                                                            Apply id [ ExpVar "Env", ExpVar "InstanceName"
                                                                     , RecordUpdate Nothing id [Assign (PatVal $ AtomicLiteral "foo") (ExpVar "bar")]
                                                                     ]
                                                        ]
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive [msi])
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs", PatVar "StateVars"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (ExpVal $ AtomicLiteral "reactiveState")
                                           ]
  , knownRebecs     = \tvds             -> PatT tvds
  , stateVars       = \tvds             -> PatT tvds
  , msgSrvInit      = \tps stms         -> Match (PatT $ (PatVal $ AtomicLiteral "initial"):tps) Nothing (foldr Call (ExpT [ExpVar "StateVars", ExpVar "LocalVars"]) stms)
  , msgSrv          = \id tps stms      -> Function "msgsrvs" [] (ExpVal $ AtomicLiteral "return this")
  , main            = \_                -> Function "main" [] (ExpVal $ AtomicLiteral "return main")
}

standardVal :: ValAlgebra String Pattern Pattern
standardVal = ValAlgebra {
    typedVarDecl    = \tvd -> case tvd of
                                R.TypedVarDecl typeName (R.Ident id) -> PatVar id
                                R.TypedVarDeclAss typeName (R.Ident id) exp -> PatVar id -- TODO
  , typedParameter  = \tp -> error "tp"
  , ident           = \id -> id    
}

standardStm :: StmAlgebra String Exp Pattern (Maybe Exp) (Maybe Exp) Exp Exp Exp Exp
standardStm = StmAlgebra {
    ass         = \id exp               -> FunAnon [PatVar "StateVars", PatVar "LocalVars"] exp
  , local       = \tvd                  -> FunAnon [PatVar "StateVars", PatVar "LocalVars"] (ExpVal $ AtomicLiteral "return this")
  , call        = \id0 id exps aft dea  -> FunAnon [PatVar "StateVars", PatVar "LocalVars"] $ case aft of -- TODO lookup id0
                                                Nothing -> Apply "tr_send" ((ExpVar id0):(ExpVal $ AtomicLiteral id):exps)
                                                Just aft' -> Apply "tr_sendafter" ((ExpVar id0):(ExpVal $ AtomicLiteral id):exps)
  , after       = \mexp                 -> mexp
  , deadline    = \mdea                 -> mdea
  , delay       = \exp                  -> FunAnon [PatVar "StateVars", PatVar "LocalVars"] (ExpVal $ AtomicLiteral "return this")
  , sel         = \exp cs elseifs els   -> FunAnon [PatVar "StateVars", PatVar "LocalVars"] (ExpVal $ AtomicLiteral "return this")
}


standardExp :: ExpAlgebra Exp
standardExp = ExpAlgebra {
    lor         = \exp0 exp -> ExpVal $ AtomicLiteral "lor"
  , land        = \exp0 exp -> ExpVal $ AtomicLiteral "land"
  , bitor       = \exp0 exp -> ExpVal $ AtomicLiteral "bitor"
  , bitexor     = \exp0 exp -> ExpVal $ AtomicLiteral "bitexor"
  , bitand      = \exp0 exp -> ExpVal $ AtomicLiteral "bitand"
  , eq          = \exp0 exp -> ExpVal $ AtomicLiteral "eq"
  , neq         = \exp0 exp -> ExpVal $ AtomicLiteral "neq"
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
  , preop       = \_ exp    -> ExpVal $ AtomicLiteral "preop" -- :: R.UnaryOperator -> exp -> exp
  , now         =              ExpVal $ AtomicLiteral "now"
  , const       = \exp      -> ExpVal $ AtomicLiteral "const"
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
    -> ExpAlgebra exp
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
  , F.preopF        =              undefined -- \aop exp -> preop eA aop exp
  , F.nowF          =              now eA
  , F.constF        = \exp      -> const eA exp
  , F.varF          = \exp      -> var eA exp
}


rebecaAlgebra = ds standardModel standardStm standardVal standardExp

