module Language.Rebeca.Translation.Erlang.Simulation where

import Control.Monad.Reader
import Control.Monad.State

import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Algebra
import Language.Rebeca.Fold
import Language.Rebeca.Translation.Erlang.Refinement

{--include("$MCERLANG_HOME/languages/erlang/src/include/state.hrl").-}
{--include("$MCERLANG_HOME/languages/erlang/src/include/state.hrl").-}
{--include("$MCERLANG_HOME/languages/erlang/src/include/node.hrl").-}


simulationAlgebra = refinementAlgebra {
    modelF = \envs rcs mai -> do
        envs' <- sequence envs
        setEnvVars envs'
        rcs' <- sequence rcs
        mai' <- mai
        moduleName <- ask
        return (Program (Module moduleName)
            [Export ["main/1"]]
            [ Import "$MCERLANG_HOME/languages/erlang/src/include/state.hrl"
            , Import "$MCERLANG_HOME/languages/erlang/src/include/state.hrl"
            , Import "$MCERLANG_HOME/languages/erlang/src/include/node.hrl" ]
            []
            (concat rcs' ++ [mai']))
    
  , reactiveClassF = \id _ kr sv msi ms -> do
        id' <- id
        kr' <- kr
        sv' <- sv
        msi' <- msi
        ms' <- sequence ms
        let initialsv = Assign (PatVar "StateVars") (Apply (ModExp "dict" "from_list") [ExpL (map (\(d, i) -> ExpT [ExpVal $ AtomicLiteral i, either (ExpVal . AtomicLiteral) (\x -> x) d]) sv')])
            initiallv = Assign (PatVar "LocalVars") (Apply (ModExp "dict" "new") [])
            probeState = Apply (ModExp "mce" "probe_state") [ExpVar "InstanceName", ExpVar "NewStateVars"]
            recurs = Apply (ExpVal $ AtomicLiteral id') [ExpVar "Env", ExpVar "InstanceName", ExpVar "KnownRebecs", ExpVar "NewStateVars"]
        return ([ Function id' [PatVar "Env", PatVar "InstanceName"] $
              Receive [ Match (PatT (map PatVar kr')) Nothing $
                Apply (ExpVal $ AtomicLiteral id') [ ExpVar "Env", ExpVar "InstanceName"
                                                   , Apply (ModExp "dict" "from_list") [ExpL (map (\k -> ExpT [ExpVal $ AtomicLiteral k, ExpVar k]) kr')]
                                                   ]]
            , Function id' [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs"] $
                Seq (Seq initialsv (Seq initiallv (Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive [msi'])))) (Seq probeState recurs)
            , Function id' [PatVar "Env", PatVar "InstanceName", PatVar "KnownRebecs", PatVar "StateVars"] $
                Seq (Seq initiallv (Assign (PatT [PatVar "NewStateVars", PatVar "_"]) (Receive ms'))) (Seq probeState recurs)
            ])

  , msgSrvF = \id tps stms -> do
        id' <- id
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = PatT [PatT [PatVar "Sender", PatVar "TT", PatVar "DL"], PatVal $ AtomicLiteral id', PatT (map (PatVar . snd) tps')]
            pred = InfixExp OpLOr (InfixExp OpEq (ExpVar "DL") (ExpVal $ AtomicLiteral "inf")) (InfixExp OpLEq (Apply (ModExp "rebeca" "now") []) (ExpVar "DL"))
            probe = Apply (ModExp "mce" "probe") [ExpVal $ AtomicLiteral "drop", ExpVal $ AtomicLiteral id']
        return (Match patterns Nothing (Case pred [ Match (PatVal $ AtomicLiteral "true") Nothing (formatReceive id' $ apply $ reverse stms')
                                                  , Match (PatVal $ AtomicLiteral "false") Nothing (Seq probe (formatDrop id' retstm))]))
}

runSimulate :: R.Model -> ReaderT CompilerConf CompilerState Program
runSimulate model = fold simulationAlgebra model

translateSimulation :: String -> R.Model -> Program
translateSimulation modelName model = evalState (runReaderT (runSimulate model) modelName) initialState

