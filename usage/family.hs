module Family
where

import qualified Data.InfiniteSet as S
import qualified Herbrand.HornClause as H
import qualified Herbrand.Model as M

-- | An example language
lang :: H.Language
lang = (c,f,r)
  where c = S.fromList [H.C "homer", H.C "bart", H.C "abe"]
        f = S.empty
        r = S.fromList [H.R "father" 2, H.R "grandfather" 2]

father :: H.Relation
father = H.R "father" 2
gFather :: H.Relation
gFather = H.R "grandfather" 2
mother :: H.Relation
mother = H.R "mother" 2
parent :: H.Relation
parent = H.R "parent" 2

homerT :: H.Term
homerT = H.BrC (H.C "homer")
bartT :: H.Term
bartT  = H.BrC (H.C "bart")
abeT :: H.Term
abeT   = H.BrC (H.C "abe")

margeT :: H.Term
margeT = H.BrC (H.C "marge")
lisaT :: H.Term
lisaT = H.BrC (H.C "lisa")

xVarT :: H.Term
xVarT = H.BrV (H.V "X")
yVarT :: H.Term
yVarT = H.BrV (H.V "Y")
zVarT :: H.Term
zVarT = H.BrV (H.V "Z")

-- | An example program
program :: H.Program
program = [
    H.HornClause (H.Formula father [homerT, bartT]) []
  , H.HornClause (H.Formula father [homerT, lisaT]) []
  , H.HornClause (H.Formula father [abeT, homerT]) []
  , H.HornClause (H.Formula mother [margeT, bartT]) []
  , H.HornClause (H.Formula mother [margeT, lisaT]) []
  , H.HornClause (H.Formula gFather [xVarT, zVarT])
                 [H.Formula father [xVarT, yVarT], H.Formula father [yVarT, zVarT]]
  , H.HornClause (H.Formula parent [xVarT, yVarT])
                 [H.Formula father [xVarT, yVarT]]
  , H.HornClause (H.Formula parent [xVarT, yVarT])
                 [H.Formula mother [xVarT, yVarT]]
  ]

firstTp :: S.Set H.Formula
firstTp = M.tpOperator lang program S.empty

secondTp :: S.Set H.Formula
secondTp = M.tpOperator lang program firstTp

thirdTp :: S.Set H.Formula
thirdTp = M.tpOperator lang program secondTp
