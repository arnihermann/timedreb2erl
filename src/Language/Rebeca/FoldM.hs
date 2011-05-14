{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.Rebeca.FoldM where

import Control.Monad

import Language.Rebeca.Absrebeca
import Language.Rebeca.AlgebraM
import Language.Rebeca.Fold

import Debug.Trace
import System.IO.Unsafe

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Ident (m id) where
    fold f (Ident s) = midentF f s

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Model (m mod) where
    fold f (Model vars classes mainbody) = join $ liftM3 (mmodelF f) (fold f vars) (fold f classes) (fold f mainbody)
    -- do { vars' <- fold f vars; classes' <- fold f classes; mainbody' <- fold f mainbody; mmodelF f vars' classes' mainbody' }

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) EnvVar (m env) where
    fold f (EnvVar tp) = join $ liftM (menvVarF f) (fold f tp)
    -- do { tp' <- fold f tp; menvVarF f tp' }

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ReactiveClass (m rc) where
    fold f (ReactiveClass name qs kr sv msi ms) = do { name' <- fold f name; kr' <- fold f kr; sv' <- fold f sv; msi' <- fold f msi; ms' <- fold f ms; mreactiveClassF f name' qs kr' sv' msi' ms'}

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) KnownRebecs (m kr) where
    fold f NoKnownRebecs = mnoKnownRebecsF f
    fold f (KnownRebecs tvds) = join $ liftM (mknownRebecsF f) (fold f tvds)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) StateVars (m sv) where
    fold f NoStateVars = mnoStateVarsF f
    fold f (StateVars tvds) = join $ liftM (mstateVarsF f) (fold f tvds)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrvInit (m msi) where
    fold f (MsgSrvInit tps stms) = join $ liftM2 (mmsgSrvInitF f) (fold f tps) (fold f stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrv (m ms) where
    fold f (MsgSrv id tps stms) = join $ liftM3 (mmsgSrvF f) (fold f id) (fold f tps) (fold f stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) VarDecl (m vd) where
    fold f (VDeclAssign id exp) = join $ liftM2 (mvDeclAssignF f) (fold f id) (fold f exp)
    fold f (VDecl id) = join $ liftM (mvDeclF f) (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedVarDecl (m tvd) where
    fold f (TypedVarDecl tn id) = join $ liftM2 (mtypedVarDeclF f) (fold f tn) (fold f id)
    fold f (TypedVarDeclAss tn id exp) = join $ liftM3 (mtypedVarDeclAssF f) (fold f tn) (fold f id) (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedParameter (m tp) where
    fold f (TypedParameter tn id) = join $ liftM2 (mtypedParameterF f) (fold f tn) (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) BasicType (m bt) where
    fold f Tint = mbasicTypeIntF f
    fold f Ttime = mbasicTypeTimeF f
    fold f Tboolean = mbasicTypeBooleanF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypeName (m tn) where
    fold f (BuiltIn bt) = join $ liftM (mbuiltInF f) (fold f bt)
    fold f (ClassType id) = join $ liftM (mclassTypeF f) (fold f id)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Stm (m stm) where
    fold f (Ass id aop exp) = join $ liftM3 (massF f) (fold f id) (fold f aop) (fold f exp)
    fold f (Local var) = join $ liftM (mlocalF f) (fold f var)
    fold f (Call id0 id exps after deadline) = join $ liftM5 (mcallF f) (fold f id0) (fold f id) (fold f exps) (fold f after) (fold f deadline)
    fold f (Delay exp) = join $ liftM (mdelayF f) (fold f exp)
    fold f (Sel exp cs elif el) = join $ liftM4 (mselF f) (fold f exp) (fold f cs) (fold f elif) (fold f el)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) CompStm (m cs) where
    fold f (SingleCompoundStm stm) = join $ liftM (msingleCompStmF f) (fold f stm)
    fold f (MultCompoundStm stms) = join $ liftM (mmultCompStmF f) (fold f stms)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) After (m aft) where
    fold f (NoAfter) = mnoAfterF f
    fold f (WithAfter exp) = join $ liftM (mwithAfterF f) (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Deadline (m dea) where
    fold f (NoDeadline) = mnoDeadlineF f
    fold f (WithDeadline exp) = join $ liftM (mwithDeadlineF f) (fold f exp)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseifStm (m eli) where
    fold f (ElseifStm exp cs) = join $ liftM2 (melseifStmF f) (fold f exp) (fold f cs)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseStm (m el) where
    fold f EmptyElseStm = memptyElseStmF f
    fold f (ElseStm cs) = join $ liftM (melseStmF f) (fold f cs)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Exp (m exp) where
    fold f (Elor exp0 exp) = join $ liftM2 (mlorF f) (fold f exp0) (fold f exp)
    fold f (Eland exp0 exp) = join $ liftM2 (mlandF f) (fold f exp0) (fold f exp)
    fold f (Ebitor exp0 exp) = join $ liftM2 (mbitorF f) (fold f exp0) (fold f exp)
    fold f (Ebitexor exp0 exp) = join $ liftM2 (mbitexorF f) (fold f exp0) (fold f exp)
    fold f (Ebitand exp0 exp) = join $ liftM2 (mbitandF f) (fold f exp0) (fold f exp)
    fold f (Eeq exp0 exp) = join $ liftM2 (meqF f) (fold f exp0) (fold f exp)
    fold f (Eneq exp0 exp) = join $ liftM2 (mneqF f) (fold f exp0) (fold f exp)
    fold f (Elthen exp0 exp) = join $ liftM2 (mlthenF f) (fold f exp0) (fold f exp)
    fold f (Egrthen exp0 exp) = join $ liftM2 (mgrthenF f) (fold f exp0) (fold f exp)
    fold f (Ele exp0 exp) = join $ liftM2 (mleF f) (fold f exp0) (fold f exp)
    fold f (Ege exp0 exp) = join $ liftM2 (mgeF f) (fold f exp0) (fold f exp)
    fold f (Eleft exp0 exp) = join $ liftM2 (mleftF f) (fold f exp0) (fold f exp)
    fold f (Eright exp0 exp) = join $ liftM2 (mrightF f) (fold f exp0) (fold f exp)
    fold f (Eplus exp0 exp) = join $ liftM2 (mplusF f) (fold f exp0) (fold f exp)
    fold f (Eminus exp0 exp) = join $ liftM2 (mminusF f) (fold f exp0) (fold f exp)
    fold f (Etimes exp0 exp) = join $ liftM2 (mtimesF f) (fold f exp0) (fold f exp)
    fold f (Ediv exp0 exp) = join $ liftM2 (mdivF f) (fold f exp0) (fold f exp)
    fold f (Emod exp0 exp) = join $ liftM2 (mmodF f) (fold f exp0) (fold f exp)
    fold f (Eexpcoercion exp) = join $ liftM (mexpcoercionF f) (fold f exp)
    fold f (ENondet exps) = join $ liftM (mnondetF f) (fold f exps)
    fold f (Epreop uop exp) = join $ liftM2 (mpreopF f) (fold f uop) (fold f exp)
    fold f Enow = mnowF f
    fold f (Econst constant) = join $ liftM (mconstF f) (fold f constant)
    fold f (Evar idents) = join $ liftM (mvarF f) (fold f idents)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Constant (m con) where
    fold f (Eint i) = mconstantIntF f i
    fold f Etrue = mconstantTrueF f
    fold f Efalse = mconstantFalseF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) UnaryOperator (m uop) where
    fold f Plus = munaryPlusF f
    fold f Negative = munaryNegativeF f
    fold f Complement = munaryComplementF f
    fold f Logicalneg = munaryLogicalNegF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) AssignmentOp (m aop) where
    fold f Assign = mopAssignF f
    fold f AssignMul = mopAssignMulF f
    fold f AssignDiv = mopAssignDivF f
    fold f AssignMod = mopAssignModF f
    fold f AssignAdd = mopAssignAddF f
    fold f AssignSub = mopAssignSubF f

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Main (m mai) where
    fold f (Main inss) = join $ liftM (mmainF f) (fold f inss)

instance Monad m => Fold (RebecaAlgebraM m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) InstanceDecl (m ins) where
    fold f (InstanceDecl tvd vds exps) = join $ liftM3 (minstanceDeclF f) (fold f tvd) (fold f vds) (fold f exps)

