{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rebeca.Fold.Monoid where

import Data.Monoid

import Language.Fold
import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

type Unify a = RebecaAlgebra a a a a a a a a a a a a a a a a a a a a a a a a a

monoidAlgebra = RebecaAlgebra {
    identF = \s -> mempty

  , modelF = \envs rcs mai -> (mconcat envs) `mappend` (mconcat rcs)

  , envVarF = \_ -> mempty

  , reactiveClassF = \id _ kr sv msi ms -> kr `mappend` sv `mappend` msi `mappend` (mconcat ms)

  , noKnownRebecsF = mempty
  , knownRebecsF = \_ -> mempty

  , noStateVarsF = mempty
  , stateVarsF = \_ -> mempty

  , msgSrvInitF = \tps stms -> (mconcat tps) `mappend` (mconcat stms)

  , msgSrvF = \_ tps stms -> (mconcat tps) `mappend` (mconcat stms)

  , vDeclAssignF = \id _ -> id
  , vDeclF = \id -> id

  , typedVarDeclF = \_ id -> id
  , typedVarDeclAssF = \_ id _ -> id

  , typedParameterF = \_ id -> id

  , basicTypeIntF = mempty
  , basicTypeTimeF = mempty
  , basicTypeBooleanF = mempty

  , builtInF = \_ -> mempty
  , classTypeF = \_ -> mempty

  , assF = \_ _ _ -> mempty
  , localF = \_ -> mempty
  , callF = \_ _ _ _ _ -> mempty
  , delayF = \_ -> mempty
  , selF = \_ _ _ _ -> mempty

  , singleCompStmF = \stm -> mempty
  , multCompStmF = \stms -> mempty

  , noAfterF = mempty
  , withAfterF = \exp -> mempty

  , noDeadlineF = mempty
  , withDeadlineF = \exp -> mempty

  , elseifStmF = \exp cs -> mempty

  , emptyElseStmF = mempty
  , elseStmF = \cs -> mempty

  , lorF = mappend
  , landF = mappend
  , bitorF = mappend
  , bitexorF = mappend
  , bitandF = mappend
  , eqF = mappend
  , neqF = mappend
  , lthenF = mappend
  , grthenF = mappend
  , leF = mappend
  , geF = mappend
  , leftF = mappend
  , rightF = mappend
  , plusF = mappend
  , minusF = mappend
  , timesF = mappend
  , divF = mappend
  , modF = mappend
  , expcoercionF = \exp -> mempty
  , nondetF = \exps -> mempty
  , preopF = \uop exp -> mempty
  , nowF = mempty
  , constF = \con -> mempty
  , varF = \vs -> mempty

  , constantIntF = \_ -> mempty
  , constantTrueF = mempty
  , constantFalseF = mempty

  , unaryPlusF = mempty
  , unaryNegativeF = mempty
  , unaryComplementF = mempty
  , unaryLogicalNegF = mempty

  , opAssignF = mempty
  , opAssignMulF = mempty
  , opAssignDivF = mempty
  , opAssignModF = mempty
  , opAssignAddF = mempty
  , opAssignSubF = mempty

  , mainF = \ins -> mempty

  , instanceDeclF = \tvd vds exps -> mempty
}

{-testMonoidAlgebra :: Model -> [String]-}
{-testMonoidAlgebra = fold monoidAlgebra-}

