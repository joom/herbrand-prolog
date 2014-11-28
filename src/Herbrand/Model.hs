module Herbrand.Model
where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H
import Data.List (nub)

-- | Herbrand base of the language, all possible atomic formulae.
-- Not implemented yet, not very necessary.
herbrandBase :: H.Language -> S.Set H.Formula
herbrandBase = undefined

-- | Returns the list of variables in the Horn clause.
findVars :: H.HornClause -> [H.Variable]
findVars (H.HornClause hd tl) = nub $ concatMap findInFormula (hd:tl)
  where findInFormula :: H.Formula -> [H.Variable]
        findInFormula (H.Formula _ terms) = map H.termVariable $ filter H.isVariable terms

-- | T_P operator is a function that builds the least Herbrand model.
-- For each clause in the program, it checks if the tail is
-- a subset of the set of formulae it is given and throws in the
-- head to the resulting set if the tail is a subset.
tpOperator :: H.Program -> S.Set H.Formula -> S.Set H.Formula
tpOperator program prev = foldl throwInHead S.empty $ filter tailInPrev program
  where tailInPrev :: H.HornClause -> Bool
        tailInPrev (H.HornClause hd tl) = all (`S.member` prev) tl
        -- TODO: handle variables
        -- facts are handled automatically because of the function `all`
        throwInHead :: S.Set H.Formula -> H.HornClause -> S.Set H.Formula
        throwInHead s (H.HornClause hd _) = S.insert hd s

-- | Creates a union of the results of T_P operators.
-- For example, tpUpTo p S.empty 2 = T_P({}) \union T_P(T_P({}))
tpUpTo :: H.Program -> S.Set H.Formula -> Int -> S.Set H.Formula
tpUpTo p s upTo = tpAux 0 s
  where tpAux n prev = if   n < upTo
                       then computed `S.union` tpAux (n + 1) computed
                       else S.empty
          where computed = tpOperator p prev

-- | Creates an infinite union of the results of T_P operators.
-- For example, leastHerbrandModel p = T_P({}) \union T_P(T_P({})) \union ...
leastHerbrandModel :: H.Program -> S.Set H.Formula
leastHerbrandModel p = tpAux S.empty
  where tpAux prev = computed `S.union` tpAux computed
          where computed = tpOperator p prev
