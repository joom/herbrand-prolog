module Herbrand.Model
where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H

-- | Herbrand base of the language, all possible atomic formulae.
herbrandBase :: H.Language -> S.Set H.Relation
herbrandBase = undefined

-- | T_P operator is a function that build the least Herbrand model.
-- For each clause in the program, it checks if the tail is
-- a subset of the set of formulae it is given and throws in the
-- head to the resulting set if the tail is a subset.
tpOperator :: H.Program -> S.Set H.Relation -> S.Set H.Relation
tpOperator = undefined

tpUpTo :: H.Program -> S.Set H.Relation -> Int -> S.Set H.Relation
tpUpTo p relSet = undefined

leastHerbrandModel :: H.Program -> S.Set H.Relation
leastHerbrandModel p = undefined
