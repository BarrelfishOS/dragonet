module LPGImplTH(
    generateFCall,
    generateFCallList,
) where

import Language.Haskell.TH

import Dragonet.ProtocolGraph
import qualified Util.GraphHelpers as GH
import qualified Data.Graph.Inductive as DGI

import Dragonet.Implementation
import LPGImpl

orE a b = InfixE (Just a) (VarE $ mkName "||") (Just b)
andE a b = InfixE (Just a) (VarE $ mkName "&&") (Just b)

boolE True = ConE $ mkName "True"
boolE False = ConE $ mkName "False"

andLE = foldl andE (boolE True)
orLE = foldl orE (boolE False)

returnE = AppE (VarE $ mkName "return")

nodeS g prefix (n,l) = BindS varP $ CondE ce te fe
    where
        ps = DGI.lpre g n
        ss = DGI.lsuc g n
        varP = TupP $ map sP ss
        sP (s,p) = VarP $ mkName ("_" ++ show n ++ p ++ show s)

        inEE (n',p) = VarE $ mkName ("_" ++ show n' ++ p ++ show n)
        ce
            | nIsFNode l && null ps = ConE $ mkName "True"
            | nIsFNode l = inEE $ head ps
            | nIsONode l = ceOp
            | otherwise = error "CNodes not supported while executing graph"
        ceOp =
            case op of
                OpAnd -> (andLE tpes) `orE` (orLE fpes)
                OpOr -> (orLE tpes) `orE` (andLE fpes)
            where
                (ONode op) = nPersonality l

        fe = returnE $ TupE $ map (const $ ConE $ mkName "False") ss
        te = DoE [BindS (VarP $ mkName "p") nodeExp,
                  NoBindS $ returnE $ tExp]

        tpes = map inEE $ filter ((== "true") . snd) ps
        fpes = map inEE $ filter ((== "false") . snd) ps

        nodeExp
            | nIsFNode l = VarE $ mkName (prefix ++ nLabel l ++ "Impl")
            | nIsONode l && op == OpAnd = boolNE $ andLE tpes
            | nIsONode l && op == OpOr = boolNE $ orLE tpes
            | otherwise = error "CNodes not supported while executing graph"
            where
                (ONode op) = nPersonality l
                retS s = returnE $ LitE $ StringL s
                boolNE e = returnE $
                            CondE e (LitE $ StringL "true")
                                    (LitE $ StringL"false")

        tExp
            | null $ nPorts l = TupE []
            | otherwise = CaseE (VarE $ mkName "p") $ map pMatch $ nPorts l
        pMatch p = Match (LitP $ StringL p)
                        (NormalB $ TupE $ map (boolE . (== p) . snd) ss) []

generateFCall :: PGraph Implementation -> String -> Exp
generateFCall g p =
    DoE (ns ++ [NoBindS $ returnE $ TupE []])
    where ns = map (nodeS g p) $ GH.topsortLN g


generateFCallList g p =  ns
    where ns = map (nodeS g p) $ GH.topsortLN g
