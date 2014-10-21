
module Dragonet.Predicate.Definitions (
    PredExpr(..),
    PredBuild(..),
    predBuildSimple,
    AtomKey,
    AtomVal
) where

import Data.List as L
import Text.Show.Functions -- show instance for functions

type AtomKey = String
type AtomVal = String

--
------- Path predicates -----
--
data PredExpr = PredAtom AtomKey AtomVal |
                PredOr  [PredExpr] |
                PredAnd [PredExpr] |
                PredNot PredExpr   |
                PredTrue | PredFalse
    deriving (Eq)


instance Show PredExpr where
    show (PredAtom node port) = "pred(" ++ node ++ "," ++ port ++ ")"
    show (PredOr l)  = "or(" ++ (L.intercalate ","  $ map show l) ++ ")"
    show (PredAnd l) = "and(" ++ (L.intercalate "," $ map show l) ++ ")"
    show (PredNot e) = "not("++ (show e) ++ ")"
    show PredTrue    = "true"
    show PredFalse   = "false"


-- predicate constructors
data PredBuild = PredBuild {
      buildAND    :: [PredExpr] -> PredExpr
    , buildOR     :: [PredExpr] -> PredExpr
    , buildNOT    :: PredExpr   -> PredExpr
    , builderName :: String
} deriving (Show)

-- just use the constructors
predBuildSimple = PredBuild {
      buildAND    = PredAnd
    , buildOR     = PredOr
    , buildNOT    = PredNot
    , builderName = "Constructors"
}
