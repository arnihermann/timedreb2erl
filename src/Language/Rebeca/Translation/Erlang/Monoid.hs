{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rebeca.Translation.Erlang.Monoid where

import Data.Monoid

import Language.Rebeca.Absrebeca
import Language.Rebeca.Fold

type Unify a = RebecaAlgebra a a a a a a a a a a a a a a a a a a a a a a a a a

{-monoidAlgebra :: (Monoid id, Monoid mod, Monoid env, Monoid rc, Monoid kr, Monoid sv, Monoid msi, Monoid ms, Monoid vd, Monoid tvd, Monoid tp, Monoid bt, Monoid tn, Monoid stm, Monoid cs, Monoid aft, Monoid dea, Monoid eli, Monoid el, Monoid exp, Monoid con, Monoid uop, Monoid aop, Monoid mai, Monoid ins) => RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins-}
{-monoidAlgebra :: (Monoid m, Monoid ms) => RebecaAlgebra m [ms] ms [ms] m [ms] m m m [ms] [ms] m m [ms] m m m [ms] m [ms] m m m [ms] m-}
{-monoidAlgebra :: Monoid m => Unify m-}
{-monoidAlgebra = RebecaAlgebra {-}
    {-identF = \s -> mempty-}

  {-, modelF = \envs rcs mai -> rcs -- envs `mappend` rcs `mappend` mai-}

  {-, envVarF = \_ -> mempty-}

  {-, reactiveClassF = \id _ kr sv msi ms -> sv-}

  {-, noKnownRebecsF = mempty-}
  {-, knownRebecsF = \_ -> mempty-}

  {-, noStateVarsF = mempty-}
  {-, stateVarsF = \_ -> mempty-}

  {-, msgSrvInitF = \tps stms -> mempty-}

  {-, msgSrvF = \id tps stms -> mempty-}

  {-, vDeclAssignF = \id exp -> mempty-}
  {-, vDeclF = \id -> mempty-}

  {-, typedVarDeclF = \tn id -> mempty-}
  {-, typedVarDeclAssF = \tn id exp -> mempty-}

  {-, typedParameterF = \tn id -> mempty-}

  {-, basicTypeIntF = mempty-}
  {-, basicTypeTimeF = mempty-}
  {-, basicTypeBooleanF = mempty-}

  {-, builtInF = \_ -> mempty-}
  {-, classTypeF = \_ -> mempty-}

  {-, assF = \id aop exp -> mempty-}
  {-, localF = \tvd -> mempty-}
  {-, callF = \id0 id exps aft dea -> mempty-}
  {-, delayF = \exp -> mempty-}
  {-, selF = \exp cs elseifs els -> mempty-}

  {-, singleCompStmF = \stm -> mempty-}
  {-, multCompStmF = \stms -> mempty-}

  {-, noAfterF = mempty-}
  {-, withAfterF = \exp -> mempty-}

  {-, noDeadlineF = mempty-}
  {-, withDeadlineF = \exp -> mempty-}

  {-, elseifStmF = \exp cs -> mempty-}

  {-, emptyElseStmF = mempty-}
  {-, elseStmF = \cs -> mempty-}

  {-, lorF = mappend-}
  {-, landF = mappend-}
  {-, bitorF = mappend-}
  {-, bitexorF = mappend-}
  {-, bitandF = mappend-}
  {-, eqF = mappend-}
  {-, neqF = mappend-}
  {-, lthenF = mappend-}
  {-, grthenF = mappend-}
  {-, leF = mappend-}
  {-, geF = mappend-}
  {-, leftF = mappend-}
  {-, rightF = mappend-}
  {-, plusF = mappend-}
  {-, minusF = mappend-}
  {-, timesF = mappend-}
  {-, divF = mappend-}
  {-, modF = mappend-}
  {-, expcoercionF = \exp -> mempty-}
  {-, nondetF = \exps -> mempty-}
  {-, preopF = \uop exp -> mempty-}
  {-, nowF = mempty-}
  {-, constF = \con -> mempty-}
  {-, varF = \vs -> mempty-}

  {-, constantIntF = \_ -> mempty-}
  {-, constantTrueF = mempty-}
  {-, constantFalseF = mempty-}

  {-, unaryPlusF = mempty-}
  {-, unaryNegativeF = mempty-}
  {-, unaryComplementF = mempty-}
  {-, unaryLogicalNegF = mempty-}

  {-, opAssignF = mempty-}
  {-, opAssignMulF = mempty-}
  {-, opAssignDivF = mempty-}
  {-, opAssignModF = mempty-}
  {-, opAssignAddF = mempty-}
  {-, opAssignSubF = mempty-}

  {-, mainF = \ins -> mempty-}

  {-, instanceDeclF = \tvd vds exps -> mempty-}
{-}-}

