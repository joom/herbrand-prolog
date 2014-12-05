-- REPL
module Main where

import Data.List (nub)
import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H
import qualified Herbrand.Model as M
import qualified Prolog.Parser as P

import System.Environment (getArgs)
import System.IO

loadProgram :: IO H.Program
loadProgram = do
    (file:_) <- getArgs
    P.readProgramFromFile file

getFormula :: IO H.Formula
getFormula = do
    putStr "?- "
    hFlush stdout
    input <- getLine
    return $ P.readFormula input

-- | Starts the REPL session.
replStart :: H.Language -> S.Set H.Formula -> IO ()
replStart lang lHM = do
    formula <- getFormula
    case M.findInFormula formula of
      [] -> putStrLn $ if   formula `S.member` lHM
                       then "yes"
                       else "no"
      _  -> putStrLn "Possible answers: " >> print correct
            where spawning = M.replaceFormulaVars lang formula
                  correct = nub $ filter (`S.member` lHM) spawning
    replStart lang lHM

main :: IO ()
main = do
    putStrLn "herbrand-prolog REPL"
    program <- loadProgram
    let lang = H.generateLanguage program
    let lHM = M.leastHerbrandModel lang program
    replStart lang lHM
