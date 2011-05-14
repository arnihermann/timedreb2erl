{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rebeca.Translation.Erlang.Variables where

import Data.Monoid

import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

import Language.Rebeca.Translation.Erlang.Monoid

stateVarsAlgebra :: Unify [String]
stateVarsAlgebra = monoidAlgebra {
    stateVarsF = \tvds -> tvds
}

knownRebecsAlgebra :: Unify [String]
knownRebecsAlgebra = monoidAlgebra {
    knownRebecsF = \tvds -> tvds
}

localVarsAlgebra :: Unify [String]
localVarsAlgebra = monoidAlgebra {
    localF = \tvd -> tvd
}

{-stateVarNames :: Model -> [String]-}
stateVarNames = fold stateVarsAlgebra

{-knownRebecNames :: Model -> [String]-}
knownRebecNames = fold knownRebecsAlgebra

{-localVarNames :: Model -> [String]-}
localVarNames = fold localVarsAlgebra

