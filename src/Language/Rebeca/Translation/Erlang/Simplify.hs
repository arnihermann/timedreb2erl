module Language.Rebeca.Translation.Erlang.Simplify where

import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

import Language.Rebeca.Translation.Erlang.Identity

simplifyAssignmentAlgebra = identityAlgebra {
   assF = \id aop exp -> case aop of
                            Assign -> Ass id aop exp
                            AssignMul -> Ass id Assign (Etimes (Evar [id]) exp)
                            AssignDiv -> Ass id Assign (Ediv (Evar [id]) exp) 
                            AssignMod -> Ass id Assign (Emod (Evar [id]) exp)
                            AssignAdd -> Ass id Assign (Eplus (Evar [id]) exp)
                            AssignSub -> Ass id Assign (Eminus (Evar [id]) exp)
}

simplifyAssignment :: Model -> Model
simplifyAssignment = fold simplifyAssignmentAlgebra

