{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.Rebeca.FoldM where

import Control.Monad

import Language.Rebeca.Absrebeca
import Language.Rebeca.AlgebraM
import Language.Rebeca.Fold


instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Ident (m id) where
    fold f (Ident s) = midentF f s

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Model (m mod) where
    fold f (Model vars classes mainbody) = do { vars' <- fold f vars; classes' <- fold f classes; mainbody' <- fold f mainbody; mmodelF f vars' classes' mainbody' }
    -- liftA3 (mmodelF f) (map (fold f) vars) (map (fold f) classes) (fold f mainbody)
    -- mmodelF f <$> (map (fold f) vars) <*> (map (fold f) classes) <*> (fold f mainbody)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) EnvVar (m env) where
    fold f (EnvVar tp) = undefined -- menvVarF f <$> (fold f tp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ReactiveClass (m rc) where
    fold f (ReactiveClass name qs kr sv msi ms) = undefined -- mreactiveClassF f <$> (fold f name) <*> qs <*> (fold f kr) <*> (fold f sv) <*> (fold f msi) <*> (map (fold f) ms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) KnownRebecs (m kr) where
    fold f NoKnownRebecs = undefined -- mnoKnownRebecsF f
    fold f (KnownRebecs tvds) = undefined -- mknownRebecsF f <$> (map (fold f) tvds)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) StateVars (m sv) where
    fold f NoStateVars = undefined -- mnoStateVarsF f
    fold f (StateVars tvds) = undefined -- mstateVarsF f <$> (map (fold f) tvds)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrvInit (m msi) where
    fold f (MsgSrvInit tps stms) = undefined -- mmsgSrvInitF f <$> (map (fold f) tps) <*> (map (fold f) stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrv (m ms) where
    fold f (MsgSrv id tps stms) = undefined -- mmsgSrvF f <$> (fold f id) <*> (map (fold f) tps) <*> (map (fold f) stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) VarDecl (m vd) where
    fold f (VDeclAssign id exp) = undefined -- mvDeclAssignF f <$> (fold f id) <*> (fold f exp)
    fold f (VDecl id) = undefined -- mvDeclF f <$> (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedVarDecl (m tvd) where
    fold f (TypedVarDecl tn id) = undefined -- mtypedVarDeclF f <$> (fold f tn) <*> (fold f id)
    fold f (TypedVarDeclAss tn id exp) = undefined -- mtypedVarDeclAssF f <$> (fold f tn) <*> (fold f id) <*> (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedParameter (m tp) where
    fold f (TypedParameter tn id) = undefined -- mtypedParameterF f <$> (fold f tn) <*> (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) BasicType (m bt) where
    fold f Tint = undefined -- mbasicTypeIntF f
    fold f Ttime = undefined -- mbasicTypeTimeF f
    fold f Tboolean = undefined -- mbasicTypeBooleanF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypeName (m tn) where
    fold f (BuiltIn bt) = undefined -- mbuiltInF f <$> (fold f bt)
    fold f (ClassType id) = undefined -- mclassTypeF f <$> (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Stm (m stm) where
    fold f (Ass id aop exp) = undefined -- massF f <$> (fold f id) <*> (fold f aop) <*> (fold f exp)
    fold f (Local var) = undefined -- mlocalF f <$> (fold f var)
    fold f (Call id0 id exps after deadline) = undefined -- mcallF f <$> (fold f id0) <*> (fold f id) <*> (map (fold f) exps) <*> (fold f after) <*> (fold f deadline)
    fold f (Delay exp) = undefined -- mdelayF f <$> (fold f exp)
    fold f (Sel exp cs elif el) = undefined -- mselF f <$> (fold f exp) <*> (fold f cs) <*> (map (fold f) elif) <*> (fold f el)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) CompStm (m cs) where
    fold f (SingleCompoundStm stm) = undefined -- msingleCompStmF f <$> (fold f stm)
    fold f (MultCompoundStm stms) = undefined -- mmultCompStmF f <$> (map (fold f) stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) After (m aft) where
    fold f (NoAfter) = undefined -- mnoAfterF f
    fold f (WithAfter exp) = undefined -- mwithAfterF f <$> (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Deadline (m dea) where
    fold f (NoDeadline) = undefined -- mnoDeadlineF f
    fold f (WithDeadline exp) = undefined -- mwithDeadlineF f <$> (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseifStm (m eli) where
    fold f (ElseifStm exp cs) = undefined -- melseifStmF f <$> (fold f exp) <*> (fold f cs)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseStm (m el) where
    fold f EmptyElseStm = undefined -- memptyElseStmF f
    fold f (ElseStm cs) = undefined -- melseStmF f <$> (fold f cs)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Exp (m exp) where
    fold f (Elor exp0 exp) = undefined -- mlorF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eland exp0 exp) = undefined -- mlandF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitor exp0 exp) = undefined -- mbitorF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitexor exp0 exp) = undefined -- mbitexorF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitand exp0 exp) = undefined -- mbitandF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eeq exp0 exp) = undefined -- meqF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eneq exp0 exp) = undefined -- mneqF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Elthen exp0 exp) = undefined -- mlthenF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Egrthen exp0 exp) = undefined -- mgrthenF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ele exp0 exp) = undefined -- mleF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ege exp0 exp) = undefined -- mgeF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eleft exp0 exp) = undefined -- mleftF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eright exp0 exp) = undefined -- mrightF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eplus exp0 exp) = undefined -- mplusF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eminus exp0 exp) = undefined -- mminusF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Etimes exp0 exp) = undefined -- mtimesF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ediv exp0 exp) = undefined -- mdivF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Emod exp0 exp) = undefined -- mmodF f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eexpcoercion exp) = undefined -- mexpcoercionF f <$> (fold f exp)
    fold f (ENondet exps) = undefined -- mnondetF f <$> (map (fold f) exps)
    fold f (Epreop uop exp) = undefined -- mpreopF f <$> (fold f uop) <*> (fold f exp)
    fold f Enow = undefined -- mnowF f
    fold f (Econst constant) = undefined -- mconstF f <$> (fold f constant)
    fold f (Evar idents) = undefined -- mvarF f <$> (map (fold f) idents)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Constant (m con) where
    fold f (Eint i) = undefined -- mconstantIntF f i
    fold f Etrue = undefined -- mconstantTrueF f
    fold f Efalse = undefined -- mconstantFalseF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) UnaryOperator (m uop) where
    fold f Plus = undefined -- munaryPlusF f
    fold f Negative = undefined -- munaryNegativeF f
    fold f Complement = undefined -- munaryComplementF f
    fold f Logicalneg = undefined -- munaryLogicalNegF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) AssignmentOp (m aop) where
    fold f Assign = undefined -- mopAssignF f
    fold f AssignMul = undefined -- mopAssignMulF f
    fold f AssignDiv = undefined -- mopAssignDivF f
    fold f AssignMod = undefined -- mopAssignModF f
    fold f AssignAdd = undefined -- mopAssignAddF f
    fold f AssignSub = undefined -- mopAssignSubF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Main (m mai) where
    fold f (Main inss) = undefined -- mmainF f <$> (map (fold f) inss)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) InstanceDecl (m ins) where
    fold f (InstanceDecl tvd vds exps) = undefined -- minstanceDeclF f <$> (fold f tvd) <*> (map (fold f) vds) <*> (map (fold f) exps)
    -- do { tvd' <- fold f tvd; vds' <- fold f vds; exps' <- fold f exps; return (minstanceDeclF f tvd' vds' exps') }



