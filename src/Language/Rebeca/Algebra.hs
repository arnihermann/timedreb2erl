module Language.Rebeca.Algebra where

import Language.Rebeca.Absrebeca

{-newtype EnvList = EnvList { unEnvList :: [EnvVar] }-}
{-newtype RcList = RcList { unRcList :: [ReactiveClass] }-}
{-newtype MsList = MsList { unMsList :: [MsgSrv] }-}
{-newtype TvdList = TvdList { unTvdList :: [TypedVarDecl] }-}
{-newtype TpList = TpList { unTpList :: [TypedParameter] }-}
{-newtype StmList = StmList { unStmList :: [Stm] }-}
{-newtype ExpList = ExpList { unExpList :: [Exp] }-}
{-newtype EliList = EliList { unEliList :: [ElseifStm] }-}
{-newtype IdList = IdList { unIdList :: [Ident] }-}
{-newtype InsList = InsList { unInsList :: [InstanceDecl] }-}
{-newtype VdList = VdList { unVdList :: [VarDecl] }-}

data RebecaAlgebra
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
        envl -- 26 rest is for monomorphic lists
        rcl -- 27
        msl -- 28
        tvdl -- 29
        tpl -- 30
        stml -- 31
        expl -- 32
        elil -- 33
        idl -- 34
        insl -- 35
        vdl -- 36
    = RebecaAlgebra {
    identF :: String -> id

  , modelF :: envl -> rcl -> mai -> mod

  , envVarF :: tp -> env

  , reactiveClassF :: id -> Integer -> kr -> sv -> msi -> msl -> rc

  , noKnownRebecsF :: kr
  , knownRebecsF :: tvdl -> kr

  , noStateVarsF :: sv
  , stateVarsF :: tvdl -> sv

  , msgSrvInitF :: tpl -> stml -> msi

  , msgSrvF :: id -> tpl -> stml -> ms

  , vDeclAssignF :: id -> exp -> vd
  , vDeclF :: id -> vd

  , typedVarDeclF :: tn -> id -> tvd
  , typedVarDeclAssF :: tn -> id -> exp -> tvd

  , typedParameterF :: tn -> id -> tp

  , basicTypeIntF :: bt
  , basicTypeTimeF :: bt
  , basicTypeBooleanF :: bt

  , builtInF :: bt -> tn
  , classTypeF :: id -> tn

  , assF :: id -> aop -> exp -> stm
  , localF :: tvd -> stm
  , callF :: id -> id -> expl -> aft -> dea -> stm
  , delayF :: exp -> stm
  , selF :: exp -> cs -> elil -> el -> stm

  , singleCompStmF :: stm -> cs
  , multCompStmF :: stml -> cs

  , noAfterF :: aft
  , withAfterF :: exp -> aft

  , noDeadlineF :: dea
  , withDeadlineF :: exp -> dea

  , elseifStmF :: exp -> cs -> eli

  , emptyElseStmF :: el
  , elseStmF :: cs -> el

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
  , nondetF :: expl -> exp
  , preopF :: uop -> exp -> exp
  , nowF :: exp
  , constF :: con -> exp
  , varF :: idl -> exp

  , constantIntF :: Integer -> con
  , constantTrueF :: con
  , constantFalseF :: con

  , unaryPlusF :: uop
  , unaryNegativeF :: uop
  , unaryComplementF :: uop
  , unaryLogicalNegF :: uop

  , opAssignF :: aop
  , opAssignMulF :: aop
  , opAssignDivF :: aop
  , opAssignModF :: aop
  , opAssignAddF :: aop
  , opAssignSubF :: aop

  , mainF :: insl -> mai

  , instanceDeclF :: tvd -> vdl -> expl -> ins

  , nilEnv :: envl
  , consEnv :: [env] -> envl
  , nilRcl :: rcl
  , consRcl :: [rc] -> rcl
  , nilMs :: msl
  , consMs :: [ms] -> msl
  , nilTvd :: tvdl
  , consTvd :: [tvd] -> tvdl
  , nilTp :: tpl
  , consTp :: [tp] -> tpl
  , nilStm :: stml
  , consStm :: [stm] -> stml
  , nilExp :: expl
  , consExp :: [exp] -> expl
  , nilEli :: elil
  , consEli :: [eli] -> elil
  , nilId :: idl
  , consId :: [id] -> idl
  , nilIns :: insl
  , consIns :: [ins] -> insl
  , nilVd :: vdl
  , consVd :: [vd] -> vdl
}

