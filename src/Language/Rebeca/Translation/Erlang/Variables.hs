{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rebeca.Translation.Erlang.Variables where

import Data.Monoid

import Language.Rebeca.Absrebeca
import Language.Rebeca.Fold

import Language.Rebeca.Translation.Erlang.Monoid

{-stateVarsAlgebra :: (Monoid id, Monoid mod, Monoid env, Monoid rc, Monoid kr, Monoid sv, Monoid msi, Monoid ms, Monoid vd, Monoid tvd, Monoid tp, Monoid bt, Monoid tn, Monoid stm, Monoid cs, Monoid aft, Monoid dea, Monoid eli, Monoid el, Monoid exp, Monoid con, Monoid uop, Monoid aop, Monoid mai, Monoid ins) => RebecaAlgebra id mod env rc kr sv msi ms vd tvd tp bt tn stm cs aft dea eli el exp con uop aop mai ins-}
stateVarsAlgebra = monoidAlgebra {
    stateVarsF = \tvds -> tvds
}

stateVarNames :: Model -> [String]
stateVarNames = foldModel stateVarsAlgebra

