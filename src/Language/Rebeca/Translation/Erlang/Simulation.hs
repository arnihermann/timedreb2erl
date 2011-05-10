module Language.Rebeca.Translation.Erlang.Simulation where

simulationAlgebra = refinementAlgebra {
    reactiveClassF = \id kr sv msi ms -> [ Function id [PatVar "Env", PatVar "InstanceName"] $
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
