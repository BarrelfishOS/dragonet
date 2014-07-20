{-# LANGUAGE OverloadedStrings #-}
module LPGConf (
    addCfgFun
) where

import qualified Dragonet.ProtocolGraph as PG
import qualified Dragonet.Configuration as C

import qualified SMTLib2 as SMT
import qualified SMTLib2.Core as SMTC
import qualified SMTLib2.BitVector as SMTBV

import Data.Functor ((<$>))
import qualified Data.List as L
import Data.Maybe
import Dragonet.Implementation.IPv4 as IP4
import Data.String (fromString)

configLPGUDPSockets :: PG.ConfFunction
configLPGUDPSockets _ inE outE cfg = concat <$> mapM addSocket tuples
    where
        PG.CVList tuples = cfg
        hasL l n = l == PG.nLabel n
        findN ps l = (fst . fst) <$> L.find (hasL l . snd . fst) ps
        Just vN = findN inE "RxL4UDPValid"
        Just vhN = findN inE "RxL4UDPValidHeaderLength"
        Just irN = findN outE "TxL4UDPInitiateResponse"
        Just cN  = findN outE "RxL4UDPUnusedPort"
        addSocket (PG.CVTuple [
                    PG.CVInt sid,
                    PG.CVMaybe msIP,
                    PG.CVMaybe msPort,
                    PG.CVMaybe mdIP,
                    PG.CVMaybe mdPort]) = do
            (dxN,_) <- C.confMNewNode $ addFSemantics $
                        PG.baseFNode filterS bports
            (fsN,_) <- C.confMNewNode $
                        PG.baseFNode ("FromSocket" ++ show sid) ["out"]
            (tsN,_) <- C.confMNewNode $ PG.nAttrAdd (PG.NAttrCustom "sink") $
                        PG.baseFNode ("ToSocket" ++ show sid) ["out","drop"]
            (vsN,_) <- C.confMNewNode $ PG.baseONode ("ValidSocket" ++ show sid)
                        bports PG.NOpAnd
            return $ map (\(a,b,p) -> (a,b,PG.Edge p)) [
                (vhN,dxN,"true"),  -- RxL4UDPValidHeaderLength -> Filter
                (dxN,vsN,"false"), -- Filter -> ValidSocket
                (dxN,vsN,"true"),  -- Filter -> ValidSocket
                (dxN,cN,"false"),  -- Filter -> Collect
                (dxN,cN,"true"),   -- Filter -> Collect
                (vN, vsN,"false"), -- RxL4UDPValid -> ValidSocket
                (vN, vsN,"true"),  -- RxL4UDPValid -> ValidSocket
                (vsN,tsN,"true"),  -- ValidSocket -> ToSocket
                (fsN,irN,"out"),   -- FromSocket -> TxL4UDPInitiateResponse
                (tsN,fsN,"out")]   -- FromSocket -> ToSocket
            where
                tuple@(_,msIP',msP,mdIP',mdP) =
                    (sid, uncvi <$> msIP, uncvi <$> msPort,
                        uncvi <$> mdIP, uncvi <$> mdPort)
                uncvi (PG.CVInt i) = i
                mbIP = maybe "*" (IP4.ipToString . fromIntegral . uncvi)
                mbPort = maybe "*" (show . uncvi)
                filterS = "Filter: UDP(" ++ mbIP msIP ++ ":" ++ mbPort msPort
                            ++ " / " ++ mbIP mdIP ++ ":" ++ mbPort mdPort ++ ")"
                ipSems n i = SMTBV.bv i 32 SMTC.===
                            SMT.app
                                (fromString $ "IP4." ++ n)
                                [SMT.app "pkt" []]
                portSems n i = SMTBV.bv i 16 SMTC.===
                            SMT.app
                                (fromString $ "UDP." ++ n)
                                [SMT.app "pkt" []]
                tSems = foldl1 SMTC.and $ catMaybes [
                        ipSems "src" <$> msIP',
                        portSems "src" <$> msP,
                        ipSems "dst" <$> mdIP',
                        portSems "dst" <$> mdP]
                fSems = SMTC.not tSems
                addFSemantics n = n {
                    PG.nSemantics = [("true",tSems),("false",fSems) ] }
                bports = ["false","true"]

addCfgFun n
    | l == "RxL4UDPCUDPSockets" = configLPGUDPSockets
    | otherwise = error $ "Unknown LPG CNode: '" ++ l ++ "'"
    where l = PG.nLabel n
