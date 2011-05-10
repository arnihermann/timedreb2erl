module Language.Rebeca.Fold where

import Language.Rebeca.Absrebeca

{-
newtype Ident
data Model
data EnvVar
data ReactiveClass
data KnownRebecs
data StateVars
data MsgSrvInit
data MsgSrv
data VarDecl
data TypedVarDecl
data TypedParameter
data BasicType
data TypeName
data Stm
data CompStm
data After
data Deadline
data ElseifStm
data ElseStm
data Exp
data Constant
data UnaryOperator
data AssignmentOp
data Main
data InstanceDecl
-}

data RebecaAlgebra
        id  -- result for idents
        mod -- result for model
        env -- result for environment variables
        rc  -- result for reactive classes
        kr  -- result for known rebecs
        sv  -- result for state vars
        msi -- result for inital message server
        ms  -- result for message servers
        vd  -- result for var decl
        tvd -- result for typed var decl
        tp  -- result for typed parameter
        bt  -- result for basic type
        tn  -- result for typename
        stm -- result for statements
        cs  -- result for composite statements
        aft -- result for after
        dea -- result for deadline
        eli -- result for else if statements
        el  -- result for else 
        exp -- result for expressions
        con -- result for constant
        uop  -- result for unary operators
        aop -- result for assignment op
        mai -- result for main
        ins -- result for instance decl
    = RebecaAlgebra {
    identF :: String -> id        
  , modelF :: [env] -> [rc] -> mai -> mod
  , envVarF :: tp -> env
  , reactiveClassF :: id -> kr -> sv -> msi -> [ms] -> rc
  , knownRebecsF :: [tvd] -> kr
  , stateVarsF :: [tvd] -> sv
  , msgSrvInitF :: [tp] -> [stm] -> msi
  , msgSrvF :: id -> [tp] -> [stm] -> ms
  , mainF :: Main -> mai

  , typedVarDeclF :: TypedVarDecl -> tvd
  , typedParameterF :: TypedParameter -> tp

  , assF :: id -> exp -> stm
  , localF :: tvd -> stm
  , callF :: id -> id -> [exp] -> aft -> dea -> stm
  , delayF :: exp -> stm
  , selF :: exp -> cs -> [eli] -> el -> stm

  , singleCompStmF :: stm -> cs
  , multCompStmF :: [stm] -> cs

  , noAfterF :: aft
  , withAfterF :: exp -> aft

  , noDeadlineF :: dea
  , withDeadlineF :: exp -> dea

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
  , nondetF :: [exp] -> exp
  , preopF :: UnaryOperator -> exp -> exp
  , nowF :: exp
  , constF :: Constant -> exp
  , varF :: [id] -> exp
}

foldIdent           :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Ident          -> id
foldModel           :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Model          -> mod
foldEnv             :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> EnvVar         -> env
foldReactiveClass   :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> ReactiveClass  -> rc
foldKnownRebecs     :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> KnownRebecs    -> kr 
foldStateVars       :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> StateVars      -> sv
foldMsgSrvInit      :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> MsgSrvInit     -> msi
foldMsgSrv          :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> MsgSrv         -> ms
foldVarDecl         :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> VarDecl        -> vd
foldTypedVarDecl    :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> TypedVarDecl   -> tvd
foldTypedParameter  :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> TypedParameter -> tp
foldBasicType       :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> BasicType      -> bt
foldTypeName        :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> TypeName       -> tn
foldStm             :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Stm            -> stm
foldCompStm         :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> CompStm        -> cs
foldAfter           :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> After          -> aft
foldDeadline        :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Deadline       -> dea
foldElseifStm       :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> ElseifStm      -> eli
foldElseStm         :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> ElseStm        -> el
foldExp             :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Exp            -> exp
foldConstant        :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Constant       -> con
foldUnaryOp         :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> UnaryOperator  -> uop
foldAssignmentOp    :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> AssignmentOp   -> aop
foldMain            :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> Main           -> mai
foldInstanceDecl    :: RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins -> InstanceDecl   -> ins

foldIdent f (Ident s) = identF f s

foldModel f (Model vars classes mainbody) = modelF f (map (foldEnv f) vars) (map (foldReactiveClass f) classes) (foldMain f mainbody)

foldEnv f (EnvVar tp) = envVarF f (foldTypedParameter f tp)

foldReactiveClass f (ReactiveClass name _ kr sv msi ms) = reactiveClassF f (foldIdent f name) (foldKnownRebecs f kr) (foldStateVars f sv) (foldMsgSrvInit f msi) (map (foldMsgSrv f) ms)

foldKnownRebecs f (KnownRebecs tvds) = knownRebecsF f (map (foldTypedVarDecl f) tvds)

foldStateVars f (StateVars tvds) = stateVarsF f (map (foldTypedVarDecl f) tvds)

foldMsgSrvInit f (MsgSrvInit tps stms) = msgSrvInitF f (map (foldTypedParameter f) tps) (map (foldStm f) stms)

foldMsgSrv f (MsgSrv id tps stms) = msgSrvF f (foldIdent f id) (map (foldTypedParameter f) tps) (map (foldStm f) stms)

foldVarDecl f _ = error "fold: var decl"

foldTypedVarDecl f tvd = typedVarDeclF f tvd

foldTypedParameter f tp = typedParameterF f tp

foldBasicType f _ = error "fold: basic type"

foldTypeName f _ = error "fold: type name"

foldStm f (Ass id op exp) = assF f (foldIdent f id) (foldExp f exp)
foldStm f (Local var) = localF f (foldTypedVarDecl f var)
foldStm f (Call id0 id exps after deadline) = callF f (foldIdent f id0) (foldIdent f id) (map (foldExp f) exps) (foldAfter f after) (foldDeadline f deadline)
foldStm f (Delay exp) = delayF f (foldExp f exp)
foldStm f (Sel exp cs elif el) = selF f (foldExp f exp) (foldCompStm f cs) (map (foldElseifStm f) elif) (foldElseStm f el)

foldCompStm f (SingleCompoundStm stm) = singleCompStmF f (foldStm f stm)
foldCompStm f (MultCompoundStm stms) = multCompStmF f (map (foldStm f) stms)

foldAfter f (NoAfter) = noAfterF f
foldAfter f (WithAfter exp) = withAfterF f (foldExp f exp)

foldDeadline f (NoDeadline) = noDeadlineF f
foldDeadline f (WithDeadline exp) = withDeadlineF f (foldExp f exp)

foldElseifStm f eli = error "fold: elseif"

foldElseStm f el = error "fold: else"

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
foldExp f (ENondet exps) = nondetF f (map (foldExp f) exps)
foldExp f (Epreop op exp) = preopF f op (foldExp f exp)
foldExp f Enow = nowF f
foldExp f (Econst constant) = constF f constant
foldExp f (Evar idents) = varF f (map (foldIdent f) idents)

foldConstant f _ = error "fold: constant"

foldUnaryOp f _ = error "fold: unary op"

foldAssignmentOp f _ = error "fold: assignment op"

foldMain f mai = mainF f mai

foldInstanceDecl f _ = error "fold: instance decl"

