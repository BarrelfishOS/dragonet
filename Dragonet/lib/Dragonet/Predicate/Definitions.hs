
module Dragonet.Predicate.Definitions (
    PredExpr(..),
    AtomKey,
    AtomVal
) where

type AtomKey = String
type AtomVal = String

data PredExpr = PredAtom AtomKey AtomVal |
                PredOr  [PredExpr] |
                PredAnd [PredExpr] |
                PredNot PredExpr   |
                PredTrue | PredFalse
    deriving (Eq)

