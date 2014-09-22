{-# LANGUAGE QuasiQuotes #-}

import qualified Dragonet.Unicorn       as U
import qualified Dragonet.Predicate     as PR
import qualified Dragonet.ProtocolGraph as PG
import qualified Util.GraphHelpers      as GH

import Dragonet.DotGenerator (toDot)

import qualified Test.HUnit             as TU
import qualified Test.QuickCheck        as QC

import qualified Data.Set               as S
import qualified Data.List              as L

import Control.Monad (liftM)

import Text.RawString.QQ (r)

import Debug.Trace (trace)
tr = flip trace


isdnfTest :: PR.PredExpr -> Bool -> TU.Test
isdnfTest expr res = TU.TestCase $ TU.assertBool errmsg t
    where res'   = PR.isDNF expr
          t      = res == res'
          errmsg = "DNF test: Expression: " ++ (show expr) ++ " expect:" ++ (show res) ++ " got:" ++ (show res')

isdnfTest' :: String -> Bool -> TU.Test
isdnfTest' s = isdnfTest (PR.parseStr_ PR.predBuildSimple s)

equivHardTest :: String -> String -> Bool -> TU.Test
equivHardTest str1 str2 res = TU.TestCase $ TU.assertBool errmsg t
    where e1   = PR.parseStr_ PR.predBuildSimple str1
          e2   = PR.parseStr_ PR.predBuildSimple str2
          res' = PR.predEquivHard e1 e2
          t    = res == res'
          errmsg = "equivHardTest: Expression1:" ++ (show str1) ++ " Expression2:" ++ (show str2) ++ " expected:" ++ (show res) ++ " got:" ++ (show res')

-- check whether a builder performs particular optimizations that lead in a
-- constant (either true or false)
constTest :: PR.PredBuild -> String -> Bool -> TU.Test
constTest bld str res = TU.TestCase $ TU.assertBool errmsg (expr `PR.predEquiv` expect)
    where expr = PR.parseStr_ PR.predBuildFold str
          expect = if res then PR.PredTrue else PR.PredFalse
          errmsg = "constant test:\n builder:" ++ (PR.builderName bld) ++ "\n input:" ++ str ++ "\n got:" ++ (show expr) ++ "\n expect: " ++ (show expect)

foldConstTest = constTest PR.predBuildFold
dnfConstTest = constTest PR.predBuildDNF

foldTest :: String -> TU.Test
foldTest str = TU.TestCase $ TU.assertBool errmsg t
    where e1 = PR.parseStr_ PR.predBuildSimple str
          e2 = PR.parseStr_ PR.predBuildFold   str
          t = PR.predEquivHard e1 e2
          errmsg = "fold test:\n" ++
                   " original Expression: " ++ str ++ "\n" ++
                   " simple expression:   " ++ (show e1) ++ "\n" ++
                   " folded expression:   " ++ (show e2)

dnfTest :: String -> TU.Test
dnfTest str = TU.TestCase $ TU.assertBool errmsg t
    where e1 = PR.parseStr_ PR.predBuildSimple str
          e2 = PR.parseStr_ PR.predBuildDNF str
          t_equiv = PR.predEquivHard e1 e2
          t_isdnf = PR.isDNF e2
          t       = t_equiv && t_isdnf
          errmsg = "dnf test:" ++
                   "\n\toriginal Expression:" ++ str ++
                   "\n\tsimple expression:  " ++ (show e1) ++
                   "\n\tdnf expression:     " ++ (show e2) ++
                   "\n\tequiv:" ++ (show t_equiv) ++ " isdnf:" ++ (show t_isdnf)

compPredAssigns :: [[PR.PredAssignment]] -> [[PR.PredAssignment]] -> Bool
compPredAssigns a b = (mkset a) == (mkset b)
    where mkset :: [[PR.PredAssignment]] -> S.Set (S.Set String)
          mkset lol = S.fromList [ S.fromList $ [ show x | x <- l] | l <- lol ]

allAssignsTest :: [(PG.NLabel, PG.NPort)] -> [[PR.PredAssignment]] -> TU.Test
allAssignsTest input expected = TU.TestCase $ TU.assertBool errmsg (expected `compPredAssigns` result)
    where result = PR.getAllAssigns input
          errmsg = "allassigns test:\ninput:\n" ++ (show input) ++ "\nexpecting:\n" ++ (showl expected) ++ "\nresult:\n " ++ (showl result)
          showl l = L.intercalate "\n " $ [ show x | x <- l ]

-- basic tests on operations
optests = [ equivHardTest "true" "false" False
         , equivHardTest "true" "true"  True
         , equivHardTest "or(pred(a,a),pred(b,b))" "or(pred(b,b),pred(a,a))" True
         , equivHardTest "not(or(pred(a,a),pred(b,b)))" "and(not(pred(a,a)), not(pred(b,b)))" True
         , equivHardTest "not(and(pred(a,a),pred(b,b)))" "and(not(pred(a,a)), not(pred(b,b)))" False
         ---
         , foldConstTest "true" True
         , foldConstTest "false" False
         , foldConstTest "or(pred(a,a),not(pred(a,a)))" True
         , foldConstTest "and(pred(a,a),not(pred(a,a)))" False
         , foldConstTest "and(pred(a,a1),pred(a,a2))" False
         ---
         , foldTest "true"
         , foldTest "false"
         , foldTest "or(pred(a,a),pred(b,b),false)"
         , foldTest "or(pred(a,a),pred(b,b),true)"
         , foldTest "and(pred(a,a),pred(b,b),false)"
         , foldTest "and(pred(a,a),pred(b,b),true)"
         , foldTest "not(or(pred(a,a),pred(b,b),false))"
         , foldTest "or(and(pred(j,j)),and(false),or(false),and(pred(c,c)))"
         , isdnfTest' "true" True
         , isdnfTest' "false" True
         , isdnfTest' "and(pred(a,a), pred(b,b))" True
         , isdnfTest' "and(pred(a,a), pred(b,b), or(pred(c,c), pred(d,d)))" False
         , isdnfTest' "or(pred(a,a), pred(b,b))" True
         , isdnfTest' "or(and(pred(a,a)), pred(b,b))" True
         , isdnfTest' "or(and(pred(a,a), pred(b,b)), or(pred(d,d), pred(c,c)))" False
         , dnfTest "true"
         , dnfTest "false"
         , dnfTest "or(pred(a,a),pred(b,b),false)"
         , dnfTest "or(pred(a,a),pred(b,b),true)"
         , dnfTest "and(pred(a,a),pred(b,b),false)"
         , dnfTest "and(pred(a,a),pred(b,b),true)"
         , dnfTest "not(or(pred(a,a),pred(b,b),false))"
         -- bugs caught by quick test
         , dnfTest "or(pred(n,n),pred(o,o),and(pred(d,d),pred(p,p)))"
         , dnfTest "not(or(and(pred(a,a),pred(c,c)),not(or(pred(e,e))),not(and(pred(y,y)))))"
         , dnfTest "and(or(pred(v,v),pred(c,c)),and(pred(m,m),pred(e,e)),or(pred(m,m),pred(n,n)))"
         , dnfTest "not(and(not(not(pred(p,p))),not(and(pred(l,l))),or(pred(r,r),pred(q,q))))"
         --
         , allAssignsTest [("a", "a"), ("a", "A")] [[("a","a",PR.PredTrue)],
                                                    [("a","A",PR.PredTrue)],
                                                    [("a","a",PR.PredFalse),("a","A",PR.PredFalse)]]
         , allAssignsTest [("a", "a"), ("b", "b")] [ [("a","a",PR.PredTrue),("b","b",PR.PredTrue)],
                                                     [("a","a",PR.PredTrue),("b","b",PR.PredFalse)],
                                                     [("a","a",PR.PredFalse),("b","b",PR.PredTrue)],
                                                     [("a","a",PR.PredFalse),("b","b",PR.PredFalse)]]
         , allAssignsTest [("a", "a"), ("a", "A"), ("b", "b")]
                          [[("a","a",PR.PredFalse),("a","A",PR.PredFalse),("b","b",PR.PredFalse)],
                          [("a","a",PR.PredFalse),("a","A",PR.PredFalse),("b","b",PR.PredTrue)],
                          [("a","a",PR.PredTrue),("b","b",PR.PredFalse)],
                          [("a","a",PR.PredTrue),("b","b",PR.PredTrue)],
                          [("a","A",PR.PredTrue),("b","b",PR.PredFalse)],
                          [("a","A",PR.PredTrue),("b","b",PR.PredTrue)]]
         ]

-- test a predicate expression in a graph
predTest :: PG.PGraph -> PG.NLabel -> String -> TU.Test
predTest g label pred_str = TU.TestCase $ TU.assertBool errmsg t
    where nodes   = GH.filterNodesByL (\x -> (PG.nLabel x) == label) g
          lnodes  = length nodes
          pred    = PR.parseStr pred_str
          node    = nodes !! 0
          mypred  = PR.nodePred g node
          pred_eq = PR.predEquivHard mypred pred
          (t, errmsg) = case lnodes of
               1 -> (pred_eq, "Predicates do not match (test:EquivHard):\n" ++
                              " expecting:" ++ (show pred) ++ "\n" ++
                              " got:      " ++ (show mypred))
               0 -> (False, "There are no nodes with label " ++ label ++ " in graph")
               _ -> (False, "There are multiple nodes with label " ++ label ++ " in graph")

-- simple graph for tests
g1 = U.strToGraph [r|
graph g1 {

    boolean A1 { port true false[A3_] }
    boolean A2 { port true false[A3_] }
    and A3_ { port true[A3] port false[] }
    node A3 {}

    boolean B { port true[B1] port false[] }
    node B1 {}

    node C0 { port o[C1 C2] port dummy[] }
    boolean C1 { port true false[C3_] }
    boolean C2 { port true false[C3_] }
    and C3_ { port true[C3] port false[] }
    node C3 {}

    node D0 { port o1[D1] port o2[D2] }
    boolean D1 { port true false[D3_] }
    boolean D2 { port true false[D3_] }
    or D3_     { port true[D3]  port false[] }
    node D3 {}

    boolean E1 { port true false[E3_] }
    boolean E2 { port true false[E3_] }
    or E3_ { port true[E3] port false[] }
    node E3 {}

    node F0 {
        port p1[F1]
        port p2[F2]

        predicate p1 "pred(Prop1,X)"
    }
    boolean F1 {
        port true false [F3_]
        predicate true "pred(Prop2,Y)"
    }
    boolean F2 { port true false [F3_] }
    and F3_ { port true[F3] port false[] }
    node F3 {}

    boolean Gsrc1 {port true false[G1]}
    boolean Gsrc2 {port true false[G1]}
    boolean Gsrc3 {port true false[G2]}
    and  G1 { port true false[G2] }
    or   G2 { port true[G]  port false[] }
    node G {}
}
|]

g1PredT = predTest g1

g1_tests = [
 g1PredT "A3" "and(pred(A1,true),pred(A2,true))",
 g1PredT "B1" "pred(B,true)",
 g1PredT "C3" "and(pred(C1,true),pred(C2,true),pred(C0,o))",
 g1PredT "D3" "or(and(pred(D0,o1),pred(D1,true)),and(pred(D0,o2),pred(D2,true)))",
 g1PredT "E3" "or(pred(E1,true),pred(E2,true))",
 g1PredT "G" "or(pred(Gsrc3,true),and(pred(Gsrc1,true),pred(Gsrc2,true)))",
 g1PredT "F1" "and(pred(Prop1,X),pred(F0,p1))",
 g1PredT "F3" "false"
 ]


tests = TU.TestList $ optests ++ g1_tests

-- https://stackoverflow.com/questions/15959357/quickcheck-arbitrary-instances-of-nested-data-structures-that-generate-balanced
-- https://stackoverflow.com/questions/1828850/how-to-use-oneof-in-quickcheck-haskell
-- http://chromaticleaves.com/posts/generate-user-data-quickcheck.html
-- http://jasani.org/2008/01/03/testing-haskell-with-quickcheck/
instance QC.Arbitrary PR.PredExpr where
    arbitrary = QC.sized arbPredExpr

fold_prop :: PR.PredExpr -> Bool
fold_prop e1 = ret
    where e2 = PR.predDoBuild PR.predBuildFold e1
          ret = PR.predEquivHard e1 e2
          ret' = tr ret ("Checking " ++ (show e1) ++ " against " ++ (show e2))

dnf_prop :: PR.PredExpr -> Bool
dnf_prop e1 = ret
    where e2 = PR.predDoBuild PR.predBuildDNF e1
          ret_equiv = PR.predEquivHard e1 e2
          ret_isdnf = PR.isDNF e2
          ret       = ret_equiv && ret_isdnf
          ret'       = tr ret $ "dnf test:\n\tsimple expression:" ++ (show e1) ++ "\n\tdnf expression:" ++ (show e2) ++ "\n\tequiv:" ++ (show ret_equiv) ++ " isdnf:" ++ (show ret_isdnf)

arbPredExpr :: Int -> QC.Gen PR.PredExpr
arbPredExpr 0 = do
    t <- QC.elements ['a'..'z']
    let gen_consts = True
    case gen_consts of
        False -> return $ PR.PredAtom [t] [t]
        True -> QC.oneof [return PR.PredTrue,
                          return PR.PredFalse,
                          return $ PR.PredAtom [t] [t]]

arbPredExpr n = do
    c <- QC.elements [1..n]
    QC.oneof [
        liftM PR.PredNot (arbPredExpr $ n-1),
        liftM PR.PredAnd $ QC.vectorOf c (arbPredExpr $ n-c),
        liftM PR.PredOr $ QC.vectorOf c (arbPredExpr $ n-c)]

main = do
    writeFile "tests/predicate-g1.dot" $ toDot g1
    TU.runTestTT tests
    --print some examples
    --QC.sample (QC.arbitrary :: QC.Gen PR.PredExpr )
    let args = QC.stdArgs {
        QC.maxSuccess = 100000,
        QC.maxSize = 7 -- keep the size small so that equivHard does not explode
    }
    QC.quickCheckWith args fold_prop
    QC.quickCheckWith args dnf_prop
    return ()
