module Language.Rebeca.Algebra where

import Language.Rebeca.Absrebeca

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
    = RebecaAlgebra {
    identF :: String -> id

  , modelF :: [env] -> [rc] -> mai -> mod

  , envVarF :: tp -> env

  , reactiveClassF :: id -> Integer -> kr -> sv -> msi -> [ms] -> rc

  , noKnownRebecsF :: kr
  , knownRebecsF :: [tvd] -> kr

  , noStateVarsF :: sv
  , stateVarsF :: [tvd] -> sv

  , msgSrvInitF :: [tp] -> [stm] -> msi

  , msgSrvF :: id -> [tp] -> [stm] -> ms

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
  , callF :: id -> id -> [exp] -> aft -> dea -> stm
  , delayF :: exp -> stm
  , selF :: exp -> cs -> [eli] -> el -> stm

  , singleCompStmF :: stm -> cs
  , multCompStmF :: [stm] -> cs

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
  , nondetF :: [exp] -> exp
  , preopF :: uop -> exp -> exp
  , nowF :: exp
  , constF :: con -> exp
  , varF :: [id] -> exp

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

  , mainF :: [ins] -> mai

  , instanceDeclF :: tvd -> [vd] -> [exp] -> ins
}

