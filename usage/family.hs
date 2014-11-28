{-
    % Equivalent of
    father(homer,bart).
    father(abe, homer).
-}
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

homerT :: H.Term
homerT = H.BrC (H.C "homer")
bartT :: H.Term
bartT  = H.BrC (H.C "bart")
abeT :: H.Term
abeT   = H.BrC (H.C "abe")

-- | An example program
program :: H.Program
program = [
    H.HornClause (H.Formula father [homerT, bartT]) []
  , H.HornClause (H.Formula father [abeT, homerT]) []
    -- grandfather should be defined with variables instead of atoms
  , H.HornClause (H.Formula gFather [abeT, bartT])
                 [H.Formula father [abeT, homerT], H.Formula father [homerT, bartT]]
  ]

firstTp :: S.Set H.Formula
firstTp = M.tpOperator program S.empty

secondTp :: S.Set H.Formula
secondTp = M.tpOperator program firstTp

thirdTp :: S.Set H.Formula
thirdTp = M.tpOperator program secondTp
