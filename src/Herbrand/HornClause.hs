module Herbrand.HornClause
where

import Prelude hiding (head, tail)
import qualified Data.InfiniteSet as S
import qualified Data.List as L

data Constant = C String     deriving (Show, Eq) -- C <const name>
data Variable = V String     deriving (Show, Eq) -- V <var name>
data Function = F String Int deriving (Show, Eq) -- F <fn name> <arity>
data Relation = R String Int deriving (Show, Eq) -- R <fn name> <arity>

type Language = (S.Set Constant, S.Set Function, S.Set Relation)

data Term = BrC {termConstant :: Constant}
          | BrV {termVariable :: Variable}
          | BrF {termFunction :: Function, termTerms :: [Term]}
          deriving (Show, Eq)

data Formula = Formula {rel :: Relation, terms :: [Term]} deriving (Show, Eq)

data HornClause = HornClause {head :: Formula, tail :: [Formula]} deriving (Show, Eq)

type Program = [HornClause]

-- | Set of all possible ground terms.
-- This needs to be defined in proper set unions, you can ignore it for now.
groundTerms :: Language -> S.Set Term
groundTerms lang@(c, f, _) = S.unions [S.map BrC c, fnSet]
  where fnSet = S.map (\fn@(F name arity) -> BrF fn terms) f
          where terms =  S.toList (groundTerms lang)

-- | Checks if the tail of a Horn clause is empty.
isFact :: HornClause -> Bool
isFact = null . tail

isVariable :: Term -> Bool
isVariable (BrV _) = True
isVariable _       = False
