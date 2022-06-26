-- | A simple graph model used to create dependecies
module Depender.DependencyGraph
  ( -- * Grap types
    Name,
    Dependencies,
    Graph,

    -- * New graph
    empty,

    -- * Manipulate graph
    addDependecies,

    -- * Extract verticies
    toList,
  )
where

import qualified Data.Bifunctor as Bf
import Data.List (nub)
import qualified Data.Map as Map

-- | Simple alias for module name
type Name = String

-- | Dependencies of module
type Dependencies = [Name]

-- | Graph type - map from module to it's dependencies
newtype Graph = MkGraph (Map.Map Name Dependencies)

-- | New empty graph
empty :: Graph
empty = MkGraph Map.empty

-- | Add an isolated vertex to graph or do nothing if it is already present
addVertex :: Graph -> Name -> Graph
addVertex (MkGraph g) name = MkGraph $ insertW g
  where
    insertW = Map.insertWith (\new old -> old) name []

-- | Add dependencies to a vertex, add the vertex if it is not present and
-- add the dependencies as new verticies if they're not present
addDependecies :: Graph -> Name -> Dependencies -> Graph
addDependecies (MkGraph g) name deps =
  flip (foldl addVertex) deps $
    MkGraph $
      Map.insertWith (++) name deps g

-- | Simple alias on one vertex
type Vertex = (Name, Dependencies)

-- | Return a list of verticies and their dependencies
toList :: Graph -> [Vertex]
toList (MkGraph g) = map (Bf.second nub) $ Map.toList g
