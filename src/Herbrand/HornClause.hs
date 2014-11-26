module Herbrand.HornClause
where

import Prelude hiding (head, tail)
import qualified Data.InfiniteSet as S
import qualified Data.List as L

data Constant = C String     deriving (Show, Eq) -- C <const name>
data Function = F String Int deriving (Show, Eq) -- F <fn name> <arity>
data Relation = R String Int deriving (Show, Eq) -- R <fn name> <arity>

data Term = BrC Constant
          | BrF Function [Term]
          deriving (Show, Eq)

type Language = (S.Set Constant, S.Set Function, S.Set Relation)

data HornClause = HornClause {head :: Relation, tail :: [Relation]}

type Program = [HornClause]

-- | An example language
-- lang1 :: Language
-- lang1 = (c,f,r)
--   where c = S.fromList [C "0", C "1", C "2"]
--         f = S.fromList [F "add" 2]
--         r = S.fromList [R "<" 2]

-- | Set of all possible ground terms.
-- This needs to be defined in proper set unions.
groundTerms :: Language -> S.Set Term
groundTerms lang@(c, f, _) = S.unions [S.map BrC c, fnSet]
  where fnSet = S.map (\fn@(F name arity) -> BrF fn terms) f
          where terms =  S.toList (groundTerms lang)

isFact :: HornClause -> Bool
isFact = null . tail
