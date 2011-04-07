module Language.Rebeca.Fold where

import Language.Rebeca.Absrebeca

data RebecaAlgebra
        mod -- result for model
        env -- result for environment variables
        rc  -- result for reactive classes
        kr  -- result for known rebecs
        sv  -- result for state vars
        msi -- result for inital message server
        ms  -- result for message servers
        exp -- result for expressions
        tvd -- result for typed var decl
        tp  -- result for typed parameter
        aft -- result for after
        dea -- result for deadline
        stm -- result for statements
        cs  -- result for composite statements
        el  -- result for else 
        eli -- result for else if statements
        id  -- result for idents
        mai -- result for main
    = RebecaAlgebra {
    modelF :: [env] -> [rc] -> mai -> mod
  , envVarF :: tp -> env
  , reactiveClassF :: id -> kr -> sv -> msi -> [ms] -> rc
  , knownRebecsF :: [tvd] -> kr
  , stateVarsF :: [tvd] -> sv
  , msgSrvInitF :: [tp] -> [stm] -> msi
  , msgSrvF :: id -> [tp] -> [stm] -> ms
  , mainF :: Main -> mai

  , assF :: id -> exp -> stm
  , localF :: tvd -> stm
  , callF :: id -> id -> [exp] -> aft -> dea -> stm
  , delayF :: exp -> stm
  , selF :: exp -> cs -> [eli] -> el -> stm

  , lorF :: exp -> exp -> exp
  , landF :: exp -> exp -> exp
  , bitorF :: exp -> exp -> exp
  , bitexorF :: exp -> exp -> exp
  , bitandF :: exp -> exp -> exp
  , eqF :: exp -> exp -> exp
  , neqF :: exp -> exp -> exp
  , lthenF :: exp -> exp -> exp
  , grthenF :: exp -> exp -> exp
  , leF :: exp -> exp -> exp
  , geF :: exp -> exp -> exp
  , leftF :: exp -> exp -> exp
  , rightF :: exp -> exp -> exp
  , plusF :: exp -> exp -> exp
  , minusF :: exp -> exp -> exp
  , timesF :: exp -> exp -> exp
  , divF :: exp -> exp -> exp
  , modF :: exp -> exp -> exp
  , expcoercionF :: exp -> exp
  , nondetF :: exp -> exp -> exp
  , preopF :: UnaryOperator -> exp -> exp
  , nowF :: exp
  , constF :: exp -> exp
  , varF :: Exp -> exp
}

foldModel                   :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Model              -> mod
foldEnv                     :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> EnvVar             -> env
foldReactiveClass           :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> ReactiveClass      -> rc
foldKnownRebecs             :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> KnownRebecs        -> kr 
foldStateVars               :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> StateVars          -> sv
foldMsgSrvInit              :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> MsgSrvInit         -> msi
foldMsgSrv                  :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> MsgSrv             -> ms
foldExp                     :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Exp                -> exp
foldTypedVarDecl            :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> TypedVarDecl       -> tvd
foldTypedParameter          :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> TypedParameter     -> tp
foldAfter                   :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> After              -> aft
foldDeadline                :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Deadline           -> dea
foldStm                     :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Stm                -> stm
foldCompStm                 :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> CompStm            -> cs
foldElseStm                 :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> ElseStm            -> el
foldElseifStm               :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> ElseifStm          -> eli
foldIdent                   :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Ident              -> id
foldMain                    :: RebecaAlgebra mod env rc kr sv msi ms exp tvd tp aft dea stm cs el eli id mai -> Main               -> mai


foldModel f (Model vars classes mainbody) = modelF f (map (foldEnv f) vars) (map (foldReactiveClass f) classes) (foldMain f mainbody)
foldEnv f (EnvVar tp) = envVarF f (foldTypedParameter f tp)
foldReactiveClass f (ReactiveClass name _ kr sv msi ms) = reactiveClassF f (foldIdent f name) (foldKnownRebecs f kr) (foldStateVars f sv) (foldMsgSrvInit f msi) (map (foldMsgSrv f) ms)
foldKnownRebecs f (KnownRebecs tvds) = knownRebecsF f (map (foldTypedVarDecl f) tvds)
foldStateVars f (StateVars tvds) = stateVarsF f (map (foldTypedVarDecl f) tvds)
foldMsgSrvInit f (MsgSrvInit tps stms) = msgSrvInitF f (map (foldTypedParameter f) tps) (map (foldStm f) stms)
foldMsgSrv f (MsgSrv id tps stms) = msgSrvF f (foldIdent f id) (map (foldTypedParameter f) tps) (map (foldStm f) stms)
foldMain f mai = mainF f mai

foldExp f (Elor exp0 exp) = lorF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eland exp0 exp) = landF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ebitor exp0 exp) = bitorF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ebitexor exp0 exp) = bitexorF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ebitand exp0 exp) = bitandF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eeq exp0 exp) = eqF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eneq exp0 exp) = neqF f (foldExp f exp0) (foldExp f exp)
foldExp f (Elthen exp0 exp) = lthenF f (foldExp f exp0) (foldExp f exp)
foldExp f (Egrthen exp0 exp) = grthenF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ele exp0 exp) = leF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ege exp0 exp) = geF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eleft exp0 exp) = leftF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eright exp0 exp) = rightF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eplus exp0 exp) = plusF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eminus exp0 exp) = minusF f (foldExp f exp0) (foldExp f exp)
foldExp f (Etimes exp0 exp) = timesF f (foldExp f exp0) (foldExp f exp)
foldExp f (Ediv exp0 exp) = divF f (foldExp f exp0) (foldExp f exp)
foldExp f (Emod exp0 exp) = modF f (foldExp f exp0) (foldExp f exp)
foldExp f (Eexpcoercion exp) = expcoercionF f (foldExp f exp)
-- foldExp f (ENondet exps) = nondetF (foldExp f exps)
-- foldExp f (Epreop op exp) = preopF (foldExp f op) (foldExp f exp)
foldExp f Enow = nowF f
-- foldExp f (Econst constant) = constF (foldExp f constant)
-- foldExp f (Evar idents) = varF (map (foldIdent f) idents)

foldTypedVarDecl f _ = undefined
foldTypedParameter f _ = undefined
foldAfter f _ = undefined
foldDeadline f _ = undefined
foldStm f (Ass id op exp) = assF f (foldIdent f id) (foldExp f exp)
foldStm f (Local var) = localF f (foldTypedVarDecl f var)
foldStm f (Call id0 id exps after deadline) = callF f (foldIdent f id0) (foldIdent f id) (map (foldExp f) exps) (foldAfter f after) (foldDeadline f deadline)
foldStm f (Delay exp) = delayF f (foldExp f exp)
foldStm f (Sel exp cs elif el) = selF f (foldExp f exp) (foldCompStm f cs) (map (foldElseifStm f) elif) (foldElseStm f el)
foldCompStm f cs = undefined
foldElseStm f el = undefined
foldElseifStm f eli = undefined

foldIdent f id = undefined

