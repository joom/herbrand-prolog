module Herbrand.Model
where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H
import Data.List (nub, partition)
import Control.Monad (replicateM)

-- | Herbrand base of the language, all possible atomic formulae.
-- Not implemented yet, not very necessary.
herbrandBase :: H.Language -> S.Set H.Formula
herbrandBase = undefined

-- | Returns the list of variables in the Horn clause.
findVars :: H.HornClause -> [H.Variable]
findVars (H.HornClause hd tl) = nub $ concatMap findInFormula (hd:tl)
  where findInFormula :: H.Formula -> [H.Variable]
        findInFormula (H.Formula _ terms) = map H.termVariable $ filter H.isVariable terms

-- | Replaces a variable term with a different term in a Horn clause.
replaceVar :: H.HornClause -> H.Term -> H.Term -> H.HornClause
replaceVar (H.HornClause hd tl) varT@(H.BrV _) new =
    H.HornClause (replaceFormula hd) (map replaceFormula tl)
  where replaceFormula :: H.Formula -> H.Formula
        replaceFormula (H.Formula r terms) =
          H.Formula r $ map (\t -> if t == varT then new else t) terms
replaceVar _ _ _ = error "You can only replace a variable."

-- | True if the Horn clause has variables.
hasVars :: H.HornClause -> Bool
hasVars = null . findVars

-- | Replaces the variables in a Horn clause with all possible ground terms.
-- Returns a list of all possible ground formulae spawning from that Horn clause.
replaceVars :: H.Language -> H.HornClause -> [H.HornClause]
replaceVars lang h@(H.HornClause hd tl) = map (applySubs h) combs
  where vars = map H.BrV $ findVars h
        gTerms = S.toList $ H.groundTerms lang
        combs :: [[(H.Term, H.Term)]]
        combs = map (zip vars) $ replicateM (length vars) gTerms
        applySubs :: H.HornClause -> [(H.Term, H.Term)] -> H.HornClause
        applySubs = foldl (\acc (vT,rT) -> replaceVar acc vT rT)

-- | Program with the non-ground clauses replaced with all possible ground
-- instances for that clause.
groundProgram :: H.Language -> H.Program -> [H.HornClause]
groundProgram l p = noVars ++ concatMap (replaceVars l) p
  where (withVars, noVars) = partition hasVars p

-- | T_P operator is a function that builds the least Herbrand model.
-- For each clause in the program, it checks if the tail is
-- a subset of the set of formulae it is given and throws in the
-- head to the resulting set if the tail is a subset.
tpOperator :: H.Language -> H.Program -> S.Set H.Formula -> S.Set H.Formula
tpOperator lang program prev =
    foldl throwInHead S.empty $ filter tailInPrev (groundProgram lang program)
  where tailInPrev :: H.HornClause -> Bool
        tailInPrev (H.HornClause hd tl) = all (`S.member` prev) tl
        -- TODO: handle variables
        -- facts are handled automatically because of the function `all`
        throwInHead :: S.Set H.Formula -> H.HornClause -> S.Set H.Formula
        throwInHead s (H.HornClause hd _) = S.insert hd s

-- | Creates a union of the results of T_P operators.
-- For example, tpUpTo p S.empty 2 = T_P({}) \union T_P(T_P({}))
tpUpTo :: H.Language -> H.Program -> S.Set H.Formula -> Int -> S.Set H.Formula
tpUpTo l p s upTo = tpAux 0 s
  where tpAux n prev = if   n < upTo
                       then computed `S.union` tpAux (n + 1) computed
                       else S.empty
          where computed = tpOperator l p prev

-- | Creates an infinite union of the results of T_P operators.
-- For example, leastHerbrandModel p = T_P({}) \union T_P(T_P({})) \union ...
leastHerbrandModel :: H.Language -> H.Program -> S.Set H.Formula
leastHerbrandModel l p = tpAux S.empty
  where tpAux prev = computed `S.union` tpAux computed
          where computed = tpOperator l p prev
