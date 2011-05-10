module Language.Rebeca.Translation.Erlang.Simulation where

import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Fold

import Language.Rebeca.Translation.Erlang.Refinement

simulationAlgebra = refinementAlgebra {
    reactiveClassF = \id _ kr sv msi ms -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
                                                Receive [ Match (PatT (map PatVar kr)) Nothing $
                                                            Apply id [ ExpVar "Env", ExpVar "InstanceName"
                                                                     , Apply "dict:from_list" (concat $ map (\k -> [ExpVal $ AtomicLiteral k, ExpVar k]) kr)
                                                                     ]
                                                        ]
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive [msi])
                                           , Function id [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs", PatVar "StateVars"] $
                                                Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive ms)
                                           ]
  , msgSrvInitF = \tps stms -> Match (PatT $ (PatVal $ AtomicLiteral "initial"):(map PatVar tps)) Nothing (ap $ reverse stms)
  , msgSrvF = \id tps stms -> Match (PatT $ (PatVal $ AtomicLiteral id):(map PatVar tps)) Nothing (ap $ reverse stms)                                           
}

translateSimulation :: R.Model -> Program
translateSimulation = foldModel simulationAlgebra

