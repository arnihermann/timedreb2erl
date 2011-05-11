{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Rebeca.Translation.Erlang.Variables where

import Data.Monoid

import Language.Rebeca.Absrebeca
import Language.Rebeca.Fold

import Language.Rebeca.Translation.Erlang.Monoid

{-stateVarsAlgebra :: Monoid m => RebecaAlgebra m m m m m m m m m m m m m m m m m m m m m m m m m-}
{-stateVarsAlgebra :: Unify [String]-}

{-stateVarsAlgebra :: RebecaAlgebra [[String]] [String] [String] [String] [[[String]]] [[[String]]] [[[String]]] [[String]] a1 tvd tp a2 tn stm cs aft dea eli el exp con uop aop mai ins-}

{-stateVarsAlgebra :: String [String] String String [[String]] [[String]] [[String]] [String] a1 tvd tp a2 tn stm cs aft dea eli el exp con uop aop mai ins-}
{-stateVarsAlgebra = monoidAlgebra {-}
    {-identF = \s -> [s]-}
  {-, stateVarsF = \tvds -> tvds-}
{-}-}

{-stateVarNames :: Model -> [String]-}
{-stateVarNames = fold stateVarsAlgebra-}

