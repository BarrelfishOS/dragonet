
module ConvertDecision (
    convertDT
    , printAbstractTree
) where


import qualified DecisionTree as DT
import qualified Data.List as DL

-- #################### Code for traversing/printing Decision ################
-- Code for traversing the data-structure
type Name = String -- the name of elements (printable)
type Position = Integer -- position of action with respect to siblings

data RootNode = RootNode {
                    rootNodeName ::  Name
                   -- , relatedAction :: Action
               }
               deriving (Show, Eq)

data Declaration = Declaration {
                    instanceName :: Name
                    , instanceType :: Name
                   -- , instanceAction :: Action
                }
               deriving (Show, Eq)

data Relation = Relation {
                    parent :: Name
                    , childPos :: Position
                    , child :: Name
                   -- , parentAction :: Action
                   -- , childAction :: Action
                }
               deriving (Show, Eq)

data AbstractTree = AbstractTree {
                rootNode :: RootNode
                , declarations :: [Declaration]
                , relations :: [Relation]
                }
               deriving (Show, Eq)

-- #################### Printing Abstract Tree ################
printRelations :: [Relation] -> String
printRelations [] = []
printRelations (x:[]) = (parent x) ++ "[" ++ (show (childPos x)) ++ "]"
                        ++ " -> " ++ (child x) ++ "\n"
printRelations (x:xs) = (printRelations [x]) ++ (printRelations xs)


printDeclarations :: [Declaration] -> String
printDeclarations [] = []
printDeclarations (x:[]) = (instanceName x) ++ " :: " ++ (instanceType x)
                                        ++ "()" ++ "\n"
printDeclarations (x:xs) = (printDeclarations [x]) ++ (printDeclarations xs)

printAbstractTree :: AbstractTree -> String
printAbstractTree tree = decls ++ rels
        where
            decls = printDeclarations $ DL.nub (declarations tree)
            rels = printRelations $ relations tree


-- #################### Convert Decision into Abstract Tree ################
convertAction :: RootNode -> Position -> DT.Action -> AbstractTree
convertAction root pos (DT.Error msg) = let
                    eleName = "ERROR"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.Processed) = let
                    eleName = "Processed"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.Dropped) = let
                    eleName = "DROPPED"
                    inst = eleName ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.InQueue qid) = let
                    eleName = "RXQueue"
                    inst = eleName ++ (show qid)
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.ToDecide des) = let
                    res = convertDT des
                    inst = rootNodeName (rootNode res)
                    rName = RootNode inst
                    decls = [] ++ (declarations res)
                    rels = [(Relation (rootNodeName root) pos inst)] ++
                            (relations res)
                in
                    AbstractTree rName decls rels

-- Processes list of action and returns their combined results
myMapper :: RootNode -> Position -> [DT.Action] -> AbstractTree
myMapper root pos [] = error "Error: list of possible actions is empty!"
myMapper root pos (x:[]) = convertAction root pos x
myMapper root pos (x:xs) = AbstractTree rName decls rels
        where
            res1 = convertAction root pos x
            resRest = myMapper root (pos + 1) xs
            rName = rootNode res1
            decls = (declarations res1) ++ (declarations resRest)
            rels = (relations res1) ++ (relations resRest)

-- Convert decision data-strucutre into graph elements
convertDT :: DT.Decision -> AbstractTree
convertDT decision = AbstractTree rName decls rels
        where
            eleName = rootNodeName $ RootNode (DT.funName (DT.selector decision))
            className = "m" ++ eleName
            rName = RootNode eleName
            results = myMapper rName 0 (DT.possibleActions decision)
            decls = [(Declaration eleName className)] ++
                (declarations results)
            rels =  relations results


