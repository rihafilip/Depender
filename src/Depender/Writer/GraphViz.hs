{-# LANGUAGE TupleSections #-}
-- | GraphViz writer package
module Depender.Writer.GraphViz where

import Data.GraphViz
import Depender.Writer (Writer (MkWriter))
import qualified Depender.DependencyGraph as G
import Data.List (nub)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

-- | Parameters
params :: GraphvizParams String String String () String
params = quickParams { isDirected = True }

-- | Transform a DependencyGraph to DotGraph
transform :: G.Graph -> DotGraph String
transform graph =
  let
    edges = G.toList graph
    verticies = map (\x -> (x,x)) $ nub $ map fst edges
    edges' = concatMap (\(v, d) -> map (v,,"") d) edges
  in graphElemsToDot params verticies edges'

-- | Actual GraphViz writer
graphVizWriter :: Writer
graphVizWriter = MkWriter $ flip T.writeFile . printDotGraph . transform
