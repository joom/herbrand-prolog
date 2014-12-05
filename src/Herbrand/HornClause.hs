module Herbrand.HornClause
where

import Prelude hiding (head, tail)
import qualified Data.InfiniteSet as S
import qualified Data.List as L

data Constant = C String     deriving (Eq) -- C <const name>
data Variable = V String     deriving (Eq) -- V <var name>
data Function = F String Int deriving (Eq) -- F <fn name> <arity>
data Relation = R String Int deriving (Eq) -- R <fn name> <arity>

type Language = (S.Set Constant, S.Set Function, S.Set Relation)

data Term = BrC {termConstant :: Constant}
          | BrV {termVariable :: Variable}
          | BrF {termFunction :: Function, termTerms :: [Term]}
          deriving (Eq)

data Formula = Formula {rel :: Relation, terms :: [Term]} deriving (Eq)

data HornClause = HornClause {head :: Formula, tail :: [Formula]} deriving (Eq)

type Program = [HornClause]

-- These are the functions that define how these data types will be
-- converted to strings.
instance Show Constant where
  show (C s) = s
instance Show Variable where
  show (V s) = s
instance Show Function where
  show (F s i) = s ++ "/" ++ show i
instance Show Relation where
  show (R s i) = s ++ "/" ++ show i
instance Show Term where
  show (BrC c) = show c
  show (BrV v) = show v
  show (BrF f ts) = show f ++ "(" ++ termsStr ++ ")"
    where termsStr = L.intercalate ", " (map show ts)
instance Show Formula where
  show (Formula rel ts) = show rel ++ "(" ++ termsStr ++ ")"
    where termsStr = L.intercalate ", " (map show ts)
instance Show HornClause where
  show (HornClause hd tl) = show hd ++ " :- " ++ tlStr
    where tlStr = L.intercalate ", " (map show tl)

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

isConstant :: Term -> Bool
isConstant (BrC _) = True
isConstant _       = False

isFunction :: Term -> Bool
isFunction (BrF _ _) = True
isFunction _         = False

-- | Generates a language from a given program.
generateLanguage :: Program -> Language
generateLanguage = combineLangs . map findLang
  where findLang :: HornClause -> Language
        findLang h = (S.fromList (findConstants h),
                      S.fromList (findFunctions h),
                      S.fromList (findRelations h))
        findConstants :: HornClause -> [Constant]
        findConstants (HornClause hd tl) = concatMap find (hd:tl)
          where find :: Formula -> [Constant]
                find (Formula _ ts) = (map termConstant . filter isConstant) ts
        findFunctions :: HornClause -> [Function]
        findFunctions (HornClause hd tl) = concatMap find (hd:tl)
          where find :: Formula -> [Function]
                find (Formula _ ts) = (map termFunction . filter isFunction) ts
                --this is incomplete, should look at the sub terms as well
        findRelations :: HornClause -> [Relation]
        findRelations (HornClause hd tl) = concatMap find (hd:tl)
          where find :: Formula -> [Relation]
                find (Formula rel _) = [rel]

        combineLang :: Language -> Language -> Language
        combineLang (c1,f1,r1) (c2,f2,r2) =
            (c1 `S.union` c2, f1 `S.union` f2, r1 `S.union` r2)
        combineLangs :: [Language] -> Language
        combineLangs = foldl1 combineLang
