module Herbrand.Model
where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H

herbrandBase :: H.Language -> S.Set H.Relation
herbrandBase = undefined

tpOperator :: H.Program -> S.Set H.Relation -> S.Set H.Relation
tpOperator = undefined

tpUpTo :: H.Program -> S.Set H.Relation -> Int -> S.Set H.Relation
tpUpTo = undefined

leastHerbrandModel :: H.Program -> S.Set H.Relation
leastHerbrandModel p = undefined
