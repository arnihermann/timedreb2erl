{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Paths_timedreb2erl (getDataFileName)

import System.Console.CmdArgs
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), dropExtension, takeFileName)

import Control.Applicative

import Language.Rebeca.Parrebeca
import Language.Rebeca.Absrebeca
import Language.Rebeca.ErrM

import qualified Language.Erlang.Pretty as P
import qualified Language.Rebeca.Fold as F
import qualified Language.Rebeca.Translation.Erlang.Refinement as R
import qualified Language.Rebeca.Translation.Erlang.Simulation as S
import qualified Language.Rebeca.Translation.Simplify as Sim
import qualified Language.Rebeca.Translation.Variables as V


data Params = Params
    { simulate :: Bool
    , monitor :: Bool
    , modelFile :: String
    , outputDir :: Maybe FilePath
    } deriving (Data, Typeable, Show)

params = cmdArgsMode $ Params
    { simulate = False 
    , monitor = False
    , modelFile = "" &= argPos 0 &= typ "FILE"
    , outputDir = Nothing &= typ "FOLDER"
    } &= program "timedreb2erl" &= summary "Timed Rebeca to erlang translator"


fromFile :: FilePath -> IO Model
fromFile f = fromString <$> readFile f
  where
    fromString s = let ts = myLexer s in case pModel ts of
        Bad _ -> error $ "Could not parse model" ++ show ts
        Ok tree -> tree

main :: IO ()
main = do
    Params{..} <- cmdArgsRun params
    mod <- fromFile modelFile
    let moduleName = (dropExtension . takeFileName) modelFile
        simplepro = Sim.simplifyAssignment mod
        translationFunction = if simulate then S.translateSimulation else R.translateRefinement
        pro = translationFunction moduleName simplepro

    case outputDir of
        Nothing -> putStrLn $ P.renderProgram pro
        Just (dir) -> do
            let translatedModel = P.renderProgram pro
                erlRebecaLib = dir </> "rebeca.erl"
                erlMonitor = dir </> "monitor.erl"
                erlFileName = dir </> moduleName <.> "erl"
            
            createDirectoryIfMissing True dir
            
            putStrLn $ "Writing erlang code to " ++ erlFileName
            writeFile erlFileName translatedModel

            putStrLn $ "Writing rebeca library code to " ++ erlRebecaLib
            getDataFileName "rebeca.erl" >>= readFile >>= writeFile erlRebecaLib

            if monitor
                then do putStrLn $ "Writing monitor template code to " ++ erlMonitor
                        getDataFileName "monitor.erl" >>= readFile >>= writeFile erlMonitor
                else return ()

