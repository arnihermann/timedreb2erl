{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import System.Console.CmdArgs
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), dropExtension)

import Control.Applicative

import Language.Rebeca.Parrebeca
import Language.Rebeca.Absrebeca
import Language.Rebeca.ErrM

import Generator.Types
import qualified Generator.Discrete as Dis
import qualified Generator.Simulation as Sim
import qualified Generator.Spirit as Spi

import qualified Language.Erlang.Pretty as P
import qualified Language.Rebeca.Fold as F
import qualified Language.Rebeca.Translation.Erlang.Refinement as R
import qualified Language.Rebeca.Translation.Erlang.Simplify as Sim
import qualified Language.Rebeca.Translation.Erlang.Simulation as S
{-import qualified Language.Rebeca.Translation.Erlang.Variables as V-}

fromFile :: FilePath -> IO Model
fromFile f = fromString <$> readFile f
  where
    fromString s = let ts = myLexer s in case pModel ts of
        Bad _ -> error $ "Could not parse model" ++ show ts
        Ok tree -> tree

runErlang :: Generator -> FilePath -> FilePath -> IO ()
runErlang gen dir f = do
    model <- fromFile f
    let (modelcode, _, _, _) = gen f model
    putStrLn ("Writing erlang code to " ++ fileName)
    writeFile fileName modelcode
  where moduleName = dropExtension f
        fileName = dir </> moduleName <.> "erl"

runRecords :: Generator -> FilePath -> FilePath -> IO ()
runRecords gen dir f = do
    model <- fromFile f
    let (_, recordcode, _, _) = gen f model
    putStrLn ("Writing records to " ++ fileName)
    writeFile fileName recordcode
  where moduleName = dropExtension f
        fileName = dir </> moduleName <.> "hrl"

runMonitor :: Generator -> FilePath -> FilePath -> IO ()
runMonitor gen dir f = do
    model <- fromFile f
    let (_, _, monitorcode, _) = gen f model
    putStrLn ("Writing monitor to " ++ fileName)
    writeFile fileName monitorcode
  where moduleName = dropExtension f
        fileName = dir </> "monitor" <.> "erl"

runRun :: Generator -> FilePath -> FilePath -> IO ()
runRun gen dir f = do
    model <- fromFile f
    let (_, _, _, runcode) = gen f model
    putStrLn ("Writing run code to " ++ fileName)
    writeFile fileName runcode
  where moduleName = dropExtension f
        fileName = dir </> "run" <.> "erl"


data Params = Params
    { genmodel :: Bool
    , genrecords :: Bool
    , genrun :: Bool
    , genmonitor :: Bool
    {-, generator :: String-}
    , modelFile :: String
    , outputDir :: FilePath
    } deriving (Data, Typeable, Show)

params = cmdArgsMode $ Params
    { genmodel = False 
    , genrecords = False
    , genrun = False
    , genmonitor = False
    {-, generator = "" &= argPos 0 &= typ "GENERATOR" -- &= help "simulation/discrete/spirit"-}
    , modelFile = "" &= argPos 1 &= typ "FILE"
    , outputDir = "." &= typ "FOLDER"
    } &= program "timedreb2erl" &= summary "Timed Rebeca to erlang translator"

run :: FilePath -> FilePath -> Bool -> (FilePath -> FilePath -> IO ()) -> IO ()
run dir file cond act = if cond then (act dir file) else return ()

generators :: [(String, Generator)]
generators = [("simulation", Sim.transModel), ("discrete", Dis.transModel), ("spirit", Spi.transModel)]

main :: IO ()
main = do
    Params{..} <- cmdArgsRun params
    mod <- fromFile modelFile
    let simplepro = Sim.simplifyAssignment mod
        {-statevars = V.stateVarNames simplepro-}
        pro = S.translateSimulation simplepro
    {-putStrLn ("Statevars: " ++ (show statevars))-}
    putStrLn "Model:"
    putStrLn $ P.renderProgram pro

    {-let Just gen = lookup generator generators-}
    {-let acts = [(genmodel, runErlang), (genrecords, runRecords), (genrun, runRun), (genmonitor, runMonitor)]-}
    {-createDirectoryIfMissing True outputDir-}
    {-mapM_ (\a -> run outputDir modelFile (fst a) (snd a gen)) acts-}

