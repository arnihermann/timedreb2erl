{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.Rebeca.Fold where

import Language.Fold
import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Ident id where
    fold f (Ident s) = identF f s

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Model mod where
    fold f (Model vars classes mainbody) = modelF f (map (fold f) vars) (map (fold f) classes) (fold f mainbody)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) EnvVar env where
    fold f (EnvVar tp) = envVarF f (fold f tp)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ReactiveClass rc where
    fold f (ReactiveClass name qs kr sv msi ms) = reactiveClassF f (fold f name) qs (fold f kr) (fold f sv) (fold f msi) (map (fold f) ms)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) KnownRebecs kr where
    fold f NoKnownRebecs = noKnownRebecsF f
    fold f (KnownRebecs tvds) = knownRebecsF f (map (fold f) tvds)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) StateVars sv where
    fold f NoStateVars = noStateVarsF f
    fold f (StateVars tvds) = stateVarsF f (map (fold f) tvds)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrvInit msi where
    fold f (MsgSrvInit tps stms) = msgSrvInitF f (map (fold f) tps) (map (fold f) stms)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrv ms where
    fold f (MsgSrv id tps stms) = msgSrvF f (fold f id) (map (fold f) tps) (map (fold f) stms)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) VarDecl vd where
    fold f (VDeclAssign id exp) = vDeclAssignF f (fold f id) (fold f exp)
    fold f (VDecl id) = vDeclF f (fold f id)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedVarDecl tvd where
    fold f (TypedVarDecl tn id) = typedVarDeclF f (fold f tn) (fold f id)
    fold f (TypedVarDeclAss tn id exp) = typedVarDeclAssF f (fold f tn) (fold f id) (fold f exp)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedParameter tp where
    fold f (TypedParameter tn id) = typedParameterF f (fold f tn) (fold f id)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) BasicType bt where
    fold f Tint = basicTypeIntF f
    fold f Ttime = basicTypeTimeF f
    fold f Tboolean = basicTypeBooleanF f

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypeName tn where
    fold f (BuiltIn bt) = builtInF f (fold f bt)
    fold f (ClassType id) = classTypeF f (fold f id)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Stm stm where
    fold f (Ass id aop exp) = assF f (fold f id) (fold f aop) (fold f exp)
    fold f (Local var) = localF f (fold f var)
    fold f (Call id0 id exps after deadline) = callF f (fold f id0) (fold f id) (map (fold f) exps) (fold f after) (fold f deadline)
    fold f (Delay exp) = delayF f (fold f exp)
    fold f (Sel exp cs elif el) = selF f (fold f exp) (fold f cs) (map (fold f) elif) (fold f el)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) CompStm cs where
    fold f (SingleCompoundStm stm) = singleCompStmF f (fold f stm)
    fold f (MultCompoundStm stms) = multCompStmF f (map (fold f) stms)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) After aft where
    fold f (NoAfter) = noAfterF f
    fold f (WithAfter exp) = withAfterF f (fold f exp)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Deadline dea where
    fold f (NoDeadline) = noDeadlineF f
    fold f (WithDeadline exp) = withDeadlineF f (fold f exp)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseifStm eli where
    fold f (ElseifStm exp cs) = elseifStmF f (fold f exp) (fold f cs)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseStm el where
    fold f EmptyElseStm = emptyElseStmF f
    fold f (ElseStm cs) = elseStmF f (fold f cs)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Exp exp where
    fold f (Elor exp0 exp) = lorF f (fold f exp0) (fold f exp)
    fold f (Eland exp0 exp) = landF f (fold f exp0) (fold f exp)
    fold f (Ebitor exp0 exp) = bitorF f (fold f exp0) (fold f exp)
    fold f (Ebitexor exp0 exp) = bitexorF f (fold f exp0) (fold f exp)
    fold f (Ebitand exp0 exp) = bitandF f (fold f exp0) (fold f exp)
    fold f (Eeq exp0 exp) = eqF f (fold f exp0) (fold f exp)
    fold f (Eneq exp0 exp) = neqF f (fold f exp0) (fold f exp)
    fold f (Elthen exp0 exp) = lthenF f (fold f exp0) (fold f exp)
    fold f (Egrthen exp0 exp) = grthenF f (fold f exp0) (fold f exp)
    fold f (Ele exp0 exp) = leF f (fold f exp0) (fold f exp)
    fold f (Ege exp0 exp) = geF f (fold f exp0) (fold f exp)
    fold f (Eleft exp0 exp) = leftF f (fold f exp0) (fold f exp)
    fold f (Eright exp0 exp) = rightF f (fold f exp0) (fold f exp)
    fold f (Eplus exp0 exp) = plusF f (fold f exp0) (fold f exp)
    fold f (Eminus exp0 exp) = minusF f (fold f exp0) (fold f exp)
    fold f (Etimes exp0 exp) = timesF f (fold f exp0) (fold f exp)
    fold f (Ediv exp0 exp) = divF f (fold f exp0) (fold f exp)
    fold f (Emod exp0 exp) = modF f (fold f exp0) (fold f exp)
    fold f (Eexpcoercion exp) = expcoercionF f (fold f exp)
    fold f (ENondet exps) = nondetF f (map (fold f) exps)
    fold f (Epreop uop exp) = preopF f (fold f uop) (fold f exp)
    fold f Enow = nowF f
    fold f (Econst constant) = constF f (fold f constant)
    fold f (Evar idents) = varF f (map (fold f) idents)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Constant con where
    fold f (Eint i) = constantIntF f i
    fold f Etrue = constantTrueF f
    fold f Efalse = constantFalseF f

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) UnaryOperator uop where
    fold f Plus = unaryPlusF f
    fold f Negative = unaryNegativeF f
    fold f Complement = unaryComplementF f
    fold f Logicalneg = unaryLogicalNegF f

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) AssignmentOp aop where
    fold f Assign = opAssignF f
    fold f AssignMul = opAssignMulF f
    fold f AssignDiv = opAssignDivF f
    fold f AssignMod = opAssignModF f
    fold f AssignAdd = opAssignAddF f
    fold f AssignSub = opAssignSubF f

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Main mai where
    fold f (Main inss) = mainF f (map (fold f) inss)

instance Fold (RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) InstanceDecl ins where
    fold f (InstanceDecl tvd vds exps) = instanceDeclF f (fold f tvd) (map (fold f) vds) (map (fold f) exps)



