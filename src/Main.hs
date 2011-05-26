{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Paths_timedreb2erl (getDataFileName)

import System.Console.CmdArgs
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), dropExtension, takeFileName)

import Control.Applicative

import Language.Rebeca.Parrebeca
import Language.Rebeca.Absrebeca
import Language.Rebeca.ErrM

import qualified Language.Erlang.Pretty as P
import qualified Language.Erlang.Fold.TimedExp as T

import qualified Language.Rebeca.Fold.Erlang.Refinement as R
import qualified Language.Rebeca.Fold.Erlang.Simulation as S
import qualified Language.Rebeca.Fold.Simplify as Sim
import qualified Language.Rebeca.Fold.Variables as V

import Text.StringTemplate
import Text.StringTemplate.GenericStandard ()

data Params = Params
    { simulate :: Bool
    , monitor :: Bool
    , timeunit :: Integer
    , modelFile :: String
    , outputDir :: Maybe FilePath
    } deriving (Data, Typeable, Show)

params = cmdArgsMode $ Params
    { simulate = False 
    , monitor = False
    , timeunit = 1000
    , modelFile = "" &= argPos 0 &= typ "FILE"
    , outputDir = Nothing &= typ "FOLDER"
    } &= program "timedreb2erl" &= summary "Timed Rebeca to erlang translator"


fromFile :: FilePath -> IO Model
fromFile f = fromString <$> readFile f
  where
    fromString s = let ts = myLexer s in case pModel ts of
        Bad err -> error $ "Could not parse model: " ++ err
        Ok tree -> tree

main :: IO ()
main = do
    Params{..} <- cmdArgsRun params
    mod <- fromFile modelFile
    let moduleName = (dropExtension . takeFileName) modelFile
        simplepro = Sim.simplifyAssignment mod
        translationFunction = if simulate then S.translateSimulation else R.translateRefinement
        pro = T.fixTimedExp $ translationFunction moduleName timeunit monitor simplepro

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
            do
                filepath <- getDataFileName "rebeca.erl"
                rebtemplate <- readFile filepath
                let tpl = newSTMP rebtemplate
                    reblib = render $ setAttribute "rtfactor" timeunit tpl
                writeFile erlRebecaLib reblib

            if monitor
                then do
                    exists <- doesFileExist erlMonitor
                    if not exists
                        then do
                            putStrLn $ "Writing monitor template code to " ++ erlMonitor
                            getDataFileName "monitor.erl" >>= readFile >>= writeFile erlMonitor
                        else putStrLn $ erlMonitor ++ " already exists, not overwriting"
                else return ()

