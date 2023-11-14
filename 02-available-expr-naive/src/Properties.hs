module Properties where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Ast as Ast
import qualified Utils as Utils

type Set = Set.Set

-- This is the property stated at the bottom of p. 37
-- of Nielson, Nielson, and Hankin, which states that the labels of
-- a statement S are the union of the initial label of S, the set
-- '{ l | (l, l') in flowOf(S) }', and also '{ l' | (l, l') in flowOf(S) }'.
isFlowInvariant :: Ast.Stmt -> Bool
isFlowInvariant stmt =
  let labels = Utils.labelsOf stmt
      entry = Utils.initialLabelOf stmt
      flowGraph = Utils.flowOf stmt
      flowIns =
        case flowGraph of
          Utils.FlowSet flows ->
            Set.map (\(Utils.Flow (l, _)) -> l) flows
      flowOuts =
        case flowGraph of
          Utils.FlowSet flows ->
            Set.map (\(Utils.Flow (_, l)) -> l) flows
      labels2 = Ast.unionLabelSets
        [ Ast.mkLabelSet [entry]
        , Ast.mkLabelSet (Set.toList flowIns)
        , Ast.mkLabelSet (Set.toList flowOuts)
        ]
  in labels == labels2

-- This is the property stated at the bottom of p. 38 of Nielson, Nielson,
-- and Hankin that the labels of a statement S are the union of the final
-- labels of S, along with the the sets '{ l | (l, l') in reverseFlowsOf(S) }'
-- and '{ l' | (l, l') in reverseFlowsOf(S) }'.
isReverseFlowInvariant :: Ast.Stmt -> Bool
isReverseFlowInvariant stmt =
  let labels = Utils.labelsOf stmt
      exits = Utils.finalLabelsOf stmt
      revFlowGraph = Utils.reverseFlowOf stmt
      flowIns =
        case revFlowGraph of
          Utils.FlowSet flows ->
            Set.map (\(Utils.Flow (l, _)) -> l) flows
      flowOuts =
        case revFlowGraph of
          Utils.FlowSet  flows ->
            Set.map (\(Utils.Flow (_, l)) -> l) flows
      labels2 = Ast.unionLabelSets
        [ exits
        , Ast.mkLabelSet (Set.toList flowIns)
        , Ast.mkLabelSet (Set.toList flowOuts)
        ]
  in labels == labels2

-- This property is defined on p. 39 of Nielson, Nielson, and Hankin.
-- A program has isolated entries if there are no labels that flow
-- into its initial label.
hasIsolatedEntries :: Ast.Stmt -> Bool
hasIsolatedEntries stmt =
  let entry = Utils.initialLabelOf stmt
      labels = Utils.labelsOf stmt
      flowGraph = Utils.flowOf stmt
      check label = Utils.isInFlowSet (Utils.mkFlow label entry) flowGraph
      result = List.filter check (Ast.labelSetToList labels)
  in List.length result == 0

-- This property is defined on p. 39 of Nielson, Nielson, and Hankin.
-- A program has isolated exits if its final labels don't flow anywhere.
hasIsolatedExits :: Ast.Stmt -> Bool
hasIsolatedExits stmt =
  let exits = Ast.labelSetToList $ Utils.finalLabelsOf stmt
      labels = Ast.labelSetToList $ Utils.labelsOf stmt
      flowGraph = Utils.flowOf stmt
      flowsSomewhere label targets =
        case targets of
          [] -> False
          x : xs ->
            if Utils.isInFlowSet (Utils.mkFlow label x) flowGraph then True
            else flowsSomewhere label xs
      check label = flowsSomewhere label labels
      result = List.filter check exits
  in List.length result == 0

-- This property is defined on p. 39 of Nielson, Nielson, and Hankin.
-- A program is uniquely labeled if each label appears only once.
-- To actually check this, we can't use 'Utils.blocksOf' to get the blocks
-- of a statement and then check that each block has a unique label, since
-- 'Utils.blocksOf' builds a set of blocks, and so if there are two identical
-- blocks, it will disgard one without alerting anybody.
-- So, we need to build not a *set* of blocks, but rather a *list* of blocks.
-- Then, we can check that each label appears only once.
isUniquelyLabeled :: Ast.Stmt -> Bool
isUniquelyLabeled stmt =
  let blocksListOf blocks theStmt =
        case theStmt of
          Ast.StmtSkip l -> Utils.BlockStmtSkip l : blocks
          Ast.StmtAssign l v e -> Utils.BlockStmtAssign l v e : blocks
          Ast.StmtIf l b stmt1 stmt2 ->
            let blocks2 = Utils.BlockBexp l b : blocks
                blocks3 = blocksListOf blocks2 stmt1
            in blocksListOf blocks3 stmt2
          Ast.StmtWhile l b body ->
            let blocks2 = Utils.BlockBexp l b : blocks
            in blocksListOf blocks2 body
          Ast.StmtSeq stmt1 stmt2 ->
            let blocks2 = blocksListOf blocks stmt1
            in blocksListOf blocks2 stmt2
      blocksList = blocksListOf [] stmt
      labels = map Utils.labelOfBlock blocksList
      isDup label =
        let matches = List.findIndices (\x -> x == label) labels
        in List.length matches > 1
      result = filter isDup labels
  in List.length result == 0

-- On p. 39 of Nielson, Nielson, and Hankin, a program is said to be
-- label consistent if any two blocks B1 and B2 with the same label l
-- implies that B1 = B2. I don't know how what B1 = B2 means. What is
-- the equality there? Syntactic equality? I don't know if that's enough.
-- So for now, let's just say that being label consistent is the same as
-- being uniquely labeled.
isLabelConsistent :: Ast.Stmt -> Bool
isLabelConsistent = isUniquelyLabeled
