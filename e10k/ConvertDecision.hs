
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
printRelations (x:[]) | p == "NIC"   = "" 
                      | otherwise    = (parent x) -- ++ "[" ++ (show (childPos x)) ++ "]"
                                       ++ " -> " ++ (child x) ++ ";\n"
  where p = (parent x)
printRelations (x:xs) = (printRelations [x]) ++ (printRelations xs)

printDeclarations :: [Declaration] -> String
printDeclarations [] = []
printDeclarations (x:[]) | n == "NIC"    = ""
                         | otherwise =  (instanceName x) ++ code
                                       ++ ";\n"
  where code
          | (instanceType x) == ""           = ""
          | otherwise                        = " [" ++ (instanceType x) ++ "]";
        n = (instanceName x)
printDeclarations (x:xs) = (printDeclarations [x]) ++ (printDeclarations xs)

printStart = "digraph {\n"

printEnd = "}\n"

printAbstractTree :: AbstractTree -> String
printAbstractTree tree = start ++ decls ++ rels ++ end
        where
          start = printStart
          end = printEnd
          decls = printDeclarations $ DL.nub (declarations tree)
          rels = printRelations $ trimAbstractTree $ relations tree
          
trimAbstractTree :: [Relation] -> [Relation]
trimAbstractTree [] = []                    
trimAbstractTree (x:xs) = x : trimAbstractTree (filter (\y -> not(x ==y)) xs)

-- #################### Convert Decision into Abstract Tree ################
convertAction :: RootNode -> Position -> DT.Decision -> AbstractTree
convertAction root pos (DT.Error msg) = let
                    inst = "ERROR"
                    eleName = inst ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst eleName)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.Processed) = let
                    inst = "Processed"
                    eleName = inst ++ "State"
                    rName = RootNode inst
                    decls = []
                    rels = []
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.Dropped) = let
                    inst = "DROPPED"
                    style = "shape=box,style=filled,color=gray" -- Dropped as red box
                    eleName = inst ++ "State"
                    rName = RootNode inst
                    decls = [(Declaration inst style)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos (DT.InQueue qid) = let
                    eleName = "RXQueue"
                    style = "shape=box,style=filled,color=gray" -- Queues are boxes
                    inst = eleName ++ (show qid)
                    rName = RootNode inst
                    decls = [(Declaration inst style)]
                    rels = [(Relation (rootNodeName root) pos inst)]
                in
                    AbstractTree rName decls rels
convertAction root pos des = let
                    res = convertDT des
                    inst = rootNodeName (rootNode res)
                    rName = RootNode inst
                    decls = [] ++ (declarations res)
                    rels = [(Relation (rootNodeName root) pos inst)] ++
                            (relations res)
                in
                    AbstractTree rName decls rels

-- Processes list of action and returns their combined results
myMapper :: RootNode -> Position -> [DT.Decision] -> AbstractTree
myMapper root pos [] = error "Error: list of possible actions is empty!"
--myMapper root pos [] = AbstractTree (rootNode root) [] []
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
            eleName = rootNodeName $ RootNode (DT.name (DT.compute decision))
            className = "m" ++ eleName
            rName = RootNode eleName
            results = myMapper rName 0 (DT.possibleActions decision)
            decls = [(Declaration eleName className)] ++
                (declarations results)
            rels =  relations results


