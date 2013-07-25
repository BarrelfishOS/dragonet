module BoolExp(
    BExp(..),
    andL,
    orL,
) where

import qualified Data.Set as S

data BExp = BEVar String |
            BENot BExp |
            BEOr BExp BExp |
            BEAnd BExp BExp
    deriving (Show,Eq)

andL :: [BExp] -> BExp
andL es = foldl1 BEAnd es

orL :: [BExp] -> BExp
orL es = foldl1 BEOr es


