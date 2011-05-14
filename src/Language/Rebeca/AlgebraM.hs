module Language.Rebeca.AlgebraM where

import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra

data Monad m => RebecaAlgebraM
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
    = RebecaAlgebraM {
    midentF :: String -> m id

  , mmodelF :: envl -> rcl -> mai -> m mod

  , menvVarF :: tp -> m env

  , mreactiveClassF :: id -> Integer -> kr -> sv -> msi -> msl -> m rc

  , mnoKnownRebecsF :: m kr
  , mknownRebecsF :: tvdl -> m kr

  , mnoStateVarsF :: m sv
  , mstateVarsF :: tvdl -> m sv

  , mmsgSrvInitF :: tpl -> stml -> m msi

  , mmsgSrvF :: id -> tpl -> stml -> m ms

  , mvDeclAssignF :: id -> exp -> m vd
  , mvDeclF :: id -> m vd

  , mtypedVarDeclF :: tn -> id -> m tvd
  , mtypedVarDeclAssF :: tn -> id -> exp -> m tvd

  , mtypedParameterF :: tn -> id -> m tp

  , mbasicTypeIntF :: m bt
  , mbasicTypeTimeF :: m bt
  , mbasicTypeBooleanF :: m bt

  , mbuiltInF :: bt -> m tn
  , mclassTypeF :: id -> m tn

  , massF :: id -> aop -> exp -> m stm
  , mlocalF :: tvd -> m stm
  , mcallF :: id -> id -> expl -> aft -> dea -> m stm
  , mdelayF :: exp -> m stm
  , mselF :: exp -> cs -> elil -> el -> m stm

  , msingleCompStmF :: stm -> m cs
  , mmultCompStmF :: stml -> m cs

  , mnoAfterF :: m aft
  , mwithAfterF :: exp -> m aft

  , mnoDeadlineF :: m dea
  , mwithDeadlineF :: exp -> m dea

  , melseifStmF :: exp -> cs -> m eli

  , memptyElseStmF :: m el
  , melseStmF :: cs -> m el

  , mlorF :: exp -> exp -> m exp
  , mlandF :: exp -> exp -> m exp
  , mbitorF :: exp -> exp -> m exp
  , mbitexorF :: exp -> exp -> m exp
  , mbitandF :: exp -> exp -> m exp
  , meqF :: exp -> exp -> m exp
  , mneqF :: exp -> exp -> m exp
  , mlthenF :: exp -> exp -> m exp
  , mgrthenF :: exp -> exp -> m exp
  , mleF :: exp -> exp -> m exp
  , mgeF :: exp -> exp -> m exp
  , mleftF :: exp -> exp -> m exp
  , mrightF :: exp -> exp -> m exp
  , mplusF :: exp -> exp -> m exp
  , mminusF :: exp -> exp -> m exp
  , mtimesF :: exp -> exp -> m exp
  , mdivF :: exp -> exp -> m exp
  , mmodF :: exp -> exp -> m exp
  , mexpcoercionF :: exp -> m exp
  , mnondetF :: expl -> m exp
  , mpreopF :: uop -> exp -> m exp
  , mnowF :: m exp
  , mconstF :: con -> m exp
  , mvarF :: idl -> m exp

  , mconstantIntF :: Integer -> m con
  , mconstantTrueF :: m con
  , mconstantFalseF :: m con

  , munaryPlusF :: m uop
  , munaryNegativeF :: m uop
  , munaryComplementF :: m uop
  , munaryLogicalNegF :: m uop

  , mopAssignF :: m aop
  , mopAssignMulF :: m aop
  , mopAssignDivF :: m aop
  , mopAssignModF :: m aop
  , mopAssignAddF :: m aop
  , mopAssignSubF :: m aop

  , mmainF :: insl -> m mai

  , minstanceDeclF :: tvd -> vdl -> expl -> m ins

  , mnilEnv :: m envl
  , mconsEnv :: [env] -> m envl
  , mnilRcl :: m rcl
  , mconsRcl :: [rc] -> m rcl
  , mnilMs :: m msl
  , mconsMs :: [ms] -> m msl
  , mnilTvd :: m tvdl
  , mconsTvd :: [tvd] -> m tvdl
  , mnilTp :: m tpl
  , mconsTp :: [tp] -> m tpl
  , mnilStm :: m stml
  , mconsStm :: [stm] -> m stml
  , mnilExp :: m expl
  , mconsExp :: [exp] -> m expl
  , mnilEli :: m elil
  , mconsEli :: [eli] -> m elil
  , mnilId :: m idl
  , mconsId :: [id] -> m idl
  , mnilIns :: m insl
  , mconsIns :: [ins] -> m insl
  , mnilVd :: m vdl
  , mconsVd :: [vd] -> m vdl
}

