module Language.Rebeca.Fold.Erlang.Simulation where

import Control.Monad.Reader
import Control.Monad.State

import Language.Fold
import Language.Erlang.Builder
import Language.Erlang.Syntax
import qualified Language.Rebeca.Absrebeca as R
import Language.Rebeca.Algebra
import Language.Rebeca.Fold
import Language.Rebeca.Fold.Erlang.Refinement


simulationAlgebra = refinementAlgebra {
    modelF = \envs rcs mai -> do
        envs' <- sequence envs
        setEnvVars envs'
        rcs' <- sequence rcs
        mai' <- mai
        moduleName <- getModuleName
        rtfactor <- getRtFactor
        monitor <- getMonitor
        let opts = [ ("monitor", tupleE [atomE "monitor", listE []])
                   , ("program", tupleE [atomE moduleName, atomE "main", listE [varE "Args"]])
                   , ("time_limit", numberE 1200)
                   , ("algorithm", tupleE [atomE "mce_alg_simulation", atomE "void"])
                   ]
        let sim = Function "simulate" [varP "Args"] (Apply (moduleE "mce" "start") [RecordCreate "mce_opts" (if monitor then opts else (drop 1 opts))])
        return (Program (Module moduleName)
            [Export ["main/1", "simulate/1"]]
            [ Import "$MCERLANG_HOME/languages/erlang/src/include/state.hrl"
            , Import "$MCERLANG_HOME/languages/erlang/src/include/process.hrl"
            , Import "$MCERLANG_HOME/languages/erlang/src/include/node.hrl"
            , Import "$MCERLANG_HOME/src/include/mce_opts.hrl" ]
            [Define "RT_FACTOR" (num rtfactor)]
            (concat rcs' ++ [mai', sim]))
    
  , reactiveClassF = \id _ kr sv msi ms -> do
        setKnownRebecs []
        setStateVars []
        id' <- id
        kr' <- kr
        setKnownRebecs kr'
        sv' <- sv
        setStateVars (map (\(_, id, _) -> id) sv')
        msi' <- msi
        ms' <- sequence ms
        let initialsv = Assign (varP "StateVars") (Apply (moduleE "dict" "from_list") [listE (map (\(_, i, d) -> tupleE [atomE i, either atomE (\x -> x) d]) sv')])
            initiallv = Assign (varP "LocalVars") (Apply (moduleE "dict" "new") [])
            probeState = Apply (moduleE "mce_erl" "probe_state") [varE "InstanceName", varE "NewStateVars"]
            recurs = Apply (atomE id') [varE "Env", varE "InstanceName", varE "KnownRebecs", varE "NewStateVars"]
        return ([ Function id' [varP "Env", varP "InstanceName"] $
              Receive [ Match (tupleP (map varP kr')) Nothing $
                Apply (atomE id') [ varE "Env", varE "InstanceName"
                                                   , Apply (moduleE "dict" "from_list") [listE (map (\k -> tupleE [atomE k, varE k]) kr')]
                                                   ]]
            , Function id' [varP "Env", varP "InstanceName", varP "KnownRebecs"] $
                Seq (Seq initialsv (Seq initiallv (Assign (tupleP [varP "NewStateVars", varP "_"]) (Receive [msi'])))) (Seq probeState recurs)
            , Function id' [varP "Env", varP "InstanceName", varP "KnownRebecs", varP "StateVars"] $
                Seq (Seq initiallv (Assign (tupleP [varP "NewStateVars", varP "_"]) (Receive ms'))) (Seq probeState recurs)
            ])

  , msgSrvF = \id tps stms -> do
        setLocalVars []
        id' <- id
        tps' <- sequence tps
        stms' <- sequence stms
        let patterns = tupleP [tupleP [varP "Sender", varP "TT", varP "DL"], atomP id', tupleP (map (varP . snd) tps')]
            pred = InfixExp OpLOr (InfixExp OpEq (varE "DL") (atomE "inf")) (InfixExp OpLEq (Apply (moduleE "rebeca" "now") []) (varE "DL"))
            probe = Apply (moduleE "mce_erl" "probe") [atomE "drop", atomE id']
        return (Match patterns Nothing (Case pred [ Match (atomP "true") Nothing (formatReceive id' $ apply $ reverse stms')
                                                  , Match (atomP "false") Nothing (Seq probe (formatDrop id' retstm))]))
}

runSimulate :: R.Model -> ReaderT CompilerConf (State CompilerState) Program
runSimulate model = fold simulationAlgebra model

translateSimulation :: String -> Integer -> Bool -> R.Model -> Program
translateSimulation modelName rtfactor monitor model = evalState (runReaderT (runSimulate model) (initialConf {moduleName = modelName, rtfactor = rtfactor, monitor = monitor })) initialState

