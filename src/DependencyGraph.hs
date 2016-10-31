module DependencyGraph (
  getUnused,
  Node
) where

import Control.Arrow (first)
import Data.Graph
import Data.Array (elems, indices)
import Java
import Java.Autowired (Autowiring(AutowireAll, Autowiring))
import qualified Data.Map.Strict as Map
import Config (getBlacklistedAnnotations, blacklistedClassSuffixes)
import Data.List (isSuffixOf)

type Node = (Result, String, [String])
type NodeWithInDegree = (Node, Int)
type ClassName = String
type Indegree = Int

getUnused :: [Result] -> [Node]
getUnused xs = removeUsed (buildAutowiredMap xs) xs

removeUsed :: Map.Map String Autowiring -> [Result] -> [Node]
removeUsed autowiredMap = removeEmpty . removeBlacklisted . getWithoutInEdges
  where getWithoutInEdges   = noInEdges . graphFromEdges' . map transformToEdges
        removeEmpty         = filter nodeHasClassName
        removeBlacklisted   = filter hasNoBlacklistedAnnotation .
                              filter (isAutowired autowiredMap) .
                              filter hasNoBlacklistedClassSuffixes

isAutowired :: Map.Map String Autowiring -> Node -> Bool
isAutowired aMap node = not $ any (`Map.member` aMap) candidates
  where candidates = fileName (fst3 node) : implements (fst3 node)

buildAutowiredMap :: [Result] -> Map.Map ClassName Autowiring
buildAutowiredMap xs = Map.fromList $ concatMap autowiredToTuple xs
  where autowiredToTuple = map toAutowiredTuple . autowired

toAutowiredTuple :: Autowiring -> (ClassName, Autowiring)
toAutowiredTuple autowiring@(AutowireAll type') = (type', autowiring)
toAutowiredTuple autowiring@(Autowiring type' _) = (type', autowiring)

noInEdges :: (Graph, Vertex -> Node) -> [Node]
noInEdges (graph, vmap) = getNodesWithoutIn nodesWithInDegree
  where getNodesWithoutIn = map getNode . filter indegreeIsZero . resolveNode vmap
        nodesWithInDegree = withIndegree graph
        indegreeIsZero = (==0) . snd

getNode :: NodeWithInDegree -> Node
getNode = fst

resolveNode :: (Vertex -> Node) -> [(Vertex, Int)] -> [NodeWithInDegree]
resolveNode vmap = map (first vmap)

withIndegree :: Graph -> [(Vertex, Indegree)]
withIndegree g = zip (indices g) (elems (indegree g))

transformToEdges :: Result -> Node
transformToEdges r = (r, fileName r, outgoingEdges)
  where outgoingEdges = references r ++ imports r ++ implements r

hasNoBlacklistedAnnotation :: Node -> Bool
hasNoBlacklistedAnnotation r = not $ any (`elem` annotations) getBlacklistedAnnotations
  where annotations = topLevelAnnotations (fst3 r) ++ methodAnnotations (fst3 r)

hasNoBlacklistedClassSuffixes :: Node -> Bool
hasNoBlacklistedClassSuffixes = not . isBlacklisted . fileName . fst3
  where isBlacklisted s = any (`isSuffixOf` s) blacklistedClassSuffixes

nodeHasClassName :: Node -> Bool
nodeHasClassName = (/= "") . fileName . fst3

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
