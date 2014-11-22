module Herbrand.HornClause
where

import qualified Data.InfiniteSet as S

data Constant = C String     deriving (Show, Eq) -- C <const name>
data Function = F String Int deriving (Show, Eq) -- F <fn name> <arity>
data Relation = R String Int deriving (Show, Eq) -- R <fn name> <arity>

data Term = BrC Constant
          | BrF Function [Term]
          deriving (Show, Eq)

type Language = (S.Set Constant, S.Set Function, S.Set Relation)

data HornClause = HornClause {hd :: Relation, tl :: [Relation]}

type Program = [HornClause]

-- | An example language
-- lang1 :: Language
-- lang1 = (c,f,r)
--   where c = S.fromList [C "0", C "1", C "2"]
--         f = S.fromList [F "add" 2]
--         r = S.fromList [R "<" 2]

groundTerms :: Language -> S.Set Term
groundTerms = undefined

isFact :: HornClause -> Bool
isFact = null . tl
