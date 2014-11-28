module Herbrand.Model
where

import Data.List (foldl')
import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H

-- | Herbrand base of the language, all possible atomic formulae.
herbrandBase :: H.Language -> S.Set H.Relation
herbrandBase = undefined

-- | T_P operator is a function that builds the least Herbrand model.
-- For each clause in the program, it checks if the tail is
-- a subset of the set of formulae it is given and throws in the
-- head to the resulting set if the tail is a subset.
tpOperator :: H.Program -> S.Set H.Relation -> S.Set H.Relation
tpOperator program prev = foldl throwInHead S.empty $ filter tailInPrev program
  where tailInPrev :: H.HornClause -> Bool
        tailInPrev (H.HornClause hd tl) = all (`S.member` prev) tl
        throwInHead :: S.Set H.Relation -> H.HornClause -> S.Set H.Relation
        throwInHead s (H.HornClause hd _) = S.insert hd s

-- | Created a union of the results of T_P operators.
-- For example, tpUpTo p S.empty 2 = T_P({}) \union T_P(T_P({}))
tpUpTo :: H.Program -> S.Set H.Relation -> Int -> S.Set H.Relation
tpUpTo p s upTo = tpAux 0 s
  where tpAux n prev = if   n < upTo
                       then computed `S.union` tpAux (n + 1) computed
                       else S.empty
          where computed = tpOperator p prev

leastHerbrandModel :: H.Program -> S.Set H.Relation
leastHerbrandModel p = undefined
