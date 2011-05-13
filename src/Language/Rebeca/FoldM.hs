{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Language.Rebeca.FoldM where

import Control.Monad

import Language.Rebeca.Absrebeca
import Language.Rebeca.Fold

data Monad m => MRebecaAlgebra
        m
        id  -- 1. result for idents
        mod -- 2. result for model
        env -- 3. result for environment variables
        rc  -- 4. result for reactive classes
        kr  -- 5. result for known rebecs
        sv  -- 6. result for state vars
        msi -- 7. result for inital message server
        ms  -- 8. result for message servers
        vd  -- 9. result for var decl
        tvd -- 10. result for typed var decl
        tp  -- 11. result for typed parameter
        bt  -- 12. result for basic type
        tn  -- 13. result for typename
        stm -- 14. result for statements
        cs  -- 15. result for composite statements
        aft -- 16. result for after
        dea -- 17. result for deadline
        eli -- 18. result for else if statements
        el  -- 19. result for else 
        exp -- 20. result for expressions
        con -- 21. result for constant
        uop  -- 22. result for unary operators
        aop -- 23. result for assignment op
        mai -- 24. result for main
        ins -- 25. result for instance decl
    = MRebecaAlgebra {
    identFM :: String -> m id

  , modelFM :: [env] -> [rc] -> mai -> m mod

  , envVarFM :: tp -> m env

  , reactiveClassFM :: id -> Integer -> kr -> sv -> msi -> [ms] -> m rc

  , noKnownRebecsFM :: m kr
  , knownRebecsFM :: [tvd] -> m kr

  , noStateVarsFM :: m sv
  , stateVarsFM :: [tvd] -> m sv

  , msgSrvInitFM :: [tp] -> [stm] -> m msi

  , msgSrvFM :: id -> [tp] -> [stm] -> m ms

  , vDeclAssignFM :: id -> exp -> m vd
  , vDeclFM :: id -> m vd

  , typedVarDeclFM :: tn -> id -> m tvd
  , typedVarDeclAssFM :: tn -> id -> exp -> m tvd

  , typedParameterFM :: tn -> id -> m tp

  , basicTypeIntFM :: m bt
  , basicTypeTimeFM :: m bt
  , basicTypeBooleanFM :: m bt

  , builtInFM :: bt -> m tn
  , classTypeFM :: id -> m tn

  , assFM :: id -> aop -> exp -> m stm
  , localFM :: tvd -> m stm
  , callFM :: id -> id -> [exp] -> aft -> dea -> m stm
  , delayFM :: exp -> m stm
  , selFM :: exp -> cs -> [eli] -> el -> m stm

  , singleCompStmFM :: stm -> m cs
  , multCompStmFM :: [stm] -> m cs

  , noAfterFM :: m aft
  , withAfterFM :: exp -> m aft

  , noDeadlineFM :: m dea
  , withDeadlineFM :: exp -> m dea

  , elseifStmFM :: exp -> cs -> m eli

  , emptyElseStmFM :: m el
  , elseStmFM :: cs -> m el

  , lorFM :: exp -> exp -> m exp
  , landFM :: exp -> exp -> m exp
  , bitorFM :: exp -> exp -> m exp
  , bitexorFM :: exp -> exp -> m exp
  , bitandFM :: exp -> exp -> m exp
  , eqFM :: exp -> exp -> m exp
  , neqFM :: exp -> exp -> m exp
  , lthenFM :: exp -> exp -> m exp
  , grthenFM :: exp -> exp -> m exp
  , leFM :: exp -> exp -> m exp
  , geFM :: exp -> exp -> m exp
  , leftFM :: exp -> exp -> m exp
  , rightFM :: exp -> exp -> m exp
  , plusFM :: exp -> exp -> m exp
  , minusFM :: exp -> exp -> m exp
  , timesFM :: exp -> exp -> m exp
  , divFM :: exp -> exp -> m exp
  , modFM :: exp -> exp -> m exp
  , expcoercionFM :: exp -> m exp
  , nondetFM :: [exp] -> m exp
  , preopFM :: uop -> exp -> m exp
  , nowFM :: m exp
  , constFM :: con -> m exp
  , varFM :: [id] -> m exp

  , constantIntFM :: Integer -> m con
  , constantTrueFM :: m con
  , constantFalseFM :: m con

  , unaryPlusFM :: m uop
  , unaryNegativeFM :: m uop
  , unaryComplementFM :: m uop
  , unaryLogicalNegFM :: m uop

  , opAssignFM :: m aop
  , opAssignMulFM :: m aop
  , opAssignDivFM :: m aop
  , opAssignModFM :: m aop
  , opAssignAddFM :: m aop
  , opAssignSubFM :: m aop

  , mainFM :: [ins] -> m mai

  , instanceDeclFM :: tvd -> [vd] -> [exp] -> m ins
}


instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Ident (m id) where
    fold f (Ident s) = identFM f s

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Model (m mod) where
    fold f (Model vars classes mainbody) = undefined
    -- liftA3 (modelFM f) (map (fold f) vars) (map (fold f) classes) (fold f mainbody)
    -- modelFM f <$> (map (fold f) vars) <*> (map (fold f) classes) <*> (fold f mainbody)
    -- do { vars' <- fold f vars; classes' <- fold f classes; mainbody' <- fold f mainbody; modelFM f vars' classes' mainbody' }

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) EnvVar (m env) where
    fold f (EnvVar tp) = undefined -- envVarFM f <$> (fold f tp)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ReactiveClass (m rc) where
    fold f (ReactiveClass name qs kr sv msi ms) = undefined -- reactiveClassFM f <$> (fold f name) <*> qs <*> (fold f kr) <*> (fold f sv) <*> (fold f msi) <*> (map (fold f) ms)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) KnownRebecs (m kr) where
    fold f NoKnownRebecs = undefined -- noKnownRebecsFM f
    fold f (KnownRebecs tvds) = undefined -- knownRebecsFM f <$> (map (fold f) tvds)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) StateVars (m sv) where
    fold f NoStateVars = undefined -- noStateVarsFM f
    fold f (StateVars tvds) = undefined -- stateVarsFM f <$> (map (fold f) tvds)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrvInit (m msi) where
    fold f (MsgSrvInit tps stms) = undefined -- msgSrvInitFM f <$> (map (fold f) tps) <*> (map (fold f) stms)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) MsgSrv (m ms) where
    fold f (MsgSrv id tps stms) = undefined -- msgSrvFM f <$> (fold f id) <*> (map (fold f) tps) <*> (map (fold f) stms)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) VarDecl (m vd) where
    fold f (VDeclAssign id exp) = undefined -- vDeclAssignFM f <$> (fold f id) <*> (fold f exp)
    fold f (VDecl id) = undefined -- vDeclFM f <$> (fold f id)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedVarDecl (m tvd) where
    fold f (TypedVarDecl tn id) = undefined -- typedVarDeclFM f <$> (fold f tn) <*> (fold f id)
    fold f (TypedVarDeclAss tn id exp) = undefined -- typedVarDeclAssFM f <$> (fold f tn) <*> (fold f id) <*> (fold f exp)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypedParameter (m tp) where
    fold f (TypedParameter tn id) = undefined -- typedParameterFM f <$> (fold f tn) <*> (fold f id)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) BasicType (m bt) where
    fold f Tint = undefined -- basicTypeIntFM f
    fold f Ttime = undefined -- basicTypeTimeFM f
    fold f Tboolean = undefined -- basicTypeBooleanFM f

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) TypeName (m tn) where
    fold f (BuiltIn bt) = undefined -- builtInFM f <$> (fold f bt)
    fold f (ClassType id) = undefined -- classTypeFM f <$> (fold f id)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Stm (m stm) where
    fold f (Ass id aop exp) = undefined -- assFM f <$> (fold f id) <*> (fold f aop) <*> (fold f exp)
    fold f (Local var) = undefined -- localFM f <$> (fold f var)
    fold f (Call id0 id exps after deadline) = undefined -- callFM f <$> (fold f id0) <*> (fold f id) <*> (map (fold f) exps) <*> (fold f after) <*> (fold f deadline)
    fold f (Delay exp) = undefined -- delayFM f <$> (fold f exp)
    fold f (Sel exp cs elif el) = undefined -- selFM f <$> (fold f exp) <*> (fold f cs) <*> (map (fold f) elif) <*> (fold f el)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) CompStm (m cs) where
    fold f (SingleCompoundStm stm) = undefined -- singleCompStmFM f <$> (fold f stm)
    fold f (MultCompoundStm stms) = undefined -- multCompStmFM f <$> (map (fold f) stms)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) After (m aft) where
    fold f (NoAfter) = undefined -- noAfterFM f
    fold f (WithAfter exp) = undefined -- withAfterFM f <$> (fold f exp)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Deadline (m dea) where
    fold f (NoDeadline) = undefined -- noDeadlineFM f
    fold f (WithDeadline exp) = undefined -- withDeadlineFM f <$> (fold f exp)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseifStm (m eli) where
    fold f (ElseifStm exp cs) = undefined -- elseifStmFM f <$> (fold f exp) <*> (fold f cs)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) ElseStm (m el) where
    fold f EmptyElseStm = undefined -- emptyElseStmFM f
    fold f (ElseStm cs) = undefined -- elseStmFM f <$> (fold f cs)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Exp (m exp) where
    fold f (Elor exp0 exp) = undefined -- lorFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eland exp0 exp) = undefined -- landFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitor exp0 exp) = undefined -- bitorFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitexor exp0 exp) = undefined -- bitexorFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ebitand exp0 exp) = undefined -- bitandFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eeq exp0 exp) = undefined -- eqFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eneq exp0 exp) = undefined -- neqFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Elthen exp0 exp) = undefined -- lthenFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Egrthen exp0 exp) = undefined -- grthenFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ele exp0 exp) = undefined -- leFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ege exp0 exp) = undefined -- geFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eleft exp0 exp) = undefined -- leftFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eright exp0 exp) = undefined -- rightFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eplus exp0 exp) = undefined -- plusFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eminus exp0 exp) = undefined -- minusFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Etimes exp0 exp) = undefined -- timesFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Ediv exp0 exp) = undefined -- divFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Emod exp0 exp) = undefined -- modFM f <$> (fold f exp0) <*> (fold f exp)
    fold f (Eexpcoercion exp) = undefined -- expcoercionFM f <$> (fold f exp)
    fold f (ENondet exps) = undefined -- nondetFM f <$> (map (fold f) exps)
    fold f (Epreop uop exp) = undefined -- preopFM f <$> (fold f uop) <*> (fold f exp)
    fold f Enow = undefined -- nowFM f
    fold f (Econst constant) = undefined -- constFM f <$> (fold f constant)
    fold f (Evar idents) = undefined -- varFM f <$> (map (fold f) idents)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Constant (m con) where
    fold f (Eint i) = undefined -- constantIntFM f i
    fold f Etrue = undefined -- constantTrueFM f
    fold f Efalse = undefined -- constantFalseFM f

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) UnaryOperator (m uop) where
    fold f Plus = undefined -- unaryPlusFM f
    fold f Negative = undefined -- unaryNegativeFM f
    fold f Complement = undefined -- unaryComplementFM f
    fold f Logicalneg = undefined -- unaryLogicalNegFM f

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) AssignmentOp (m aop) where
    fold f Assign = undefined -- opAssignFM f
    fold f AssignMul = undefined -- opAssignMulFM f
    fold f AssignDiv = undefined -- opAssignDivFM f
    fold f AssignMod = undefined -- opAssignModFM f
    fold f AssignAdd = undefined -- opAssignAddFM f
    fold f AssignSub = undefined -- opAssignSubFM f

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) Main (m mai) where
    fold f (Main inss) = undefined -- mainFM f <$> (map (fold f) inss)

instance Monad m => Fold (MRebecaAlgebra m id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins) InstanceDecl (m ins) where
    fold f (InstanceDecl tvd vds exps) = undefined -- instanceDeclFM f <$> (fold f tvd) <*> (map (fold f) vds) <*> (map (fold f) exps)
    -- do { tvd' <- fold f tvd; vds' <- fold f vds; exps' <- fold f exps; return (instanceDeclFM f tvd' vds' exps') }



