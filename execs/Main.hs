-- REPL
module Main where

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

-- | Takes the least Herbrand model and starts the REPL session with it.
replStart :: S.Set H.Formula -> IO ()
replStart lHM = do
    formula <- getFormula
    -- TODO: handle variables
    if formula `S.member` lHM
    then putStrLn "yes"
    else putStrLn "no"
    replStart lHM

main :: IO ()
main = do
    putStrLn "herbrand-prolog REPL"
    program <- loadProgram
    let lang = H.generateLanguage program
    let lHM = M.leastHerbrandModel lang program
    replStart lHM
