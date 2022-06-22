{-# LANGUAGE OverloadedStrings #-}

module Depender.Writer.Mermaid (mermaidWriter) where
import Depender.Writer (Writer(MkWriter))
import qualified Depender.DependencyGraph as G
import qualified Data.Map as Map
import qualified Data.Text as T
import Depender.DependencyGraph (Name)
import Data.Maybe (fromJust)

-- Simple aliases
type IdMap = Map.Map String String
type OutString = String

-- | Replace a quote in string to a non-breaking quote
escapeText :: String -> String
escapeText = T.unpack . T.replace "\"" "#quot;" . T.pack

-- | Defines a styling on vertex
formatVertex :: Name -> String -> String
formatVertex name idName = "\t" ++ idName ++ "(\"" ++ escapeText name ++ "\");\n"

-- | Defines a styling on edge
formatEdge :: Name -> Name -> OutString
formatEdge from to = "\t" ++ from ++ " --> " ++ to ++ ";\n"

-- | On a list of verticies return a definition
defineVerticies :: [Name] -> (OutString, IdMap)
defineVerticies = foldl go ("", Map.empty) . zip [0..]
  where
    go (outp, map) (id, name) =
      let
        idName = "id" ++ show id
        newOutp = outp ++ formatVertex name idName
        newMap = Map.insert name idName map
      in (newOutp, newMap)

-- | Add a transition to output
addTransition :: IdMap -> Name -> Name -> OutString
addTransition map from to = fromJust $ do
  fromId <- Map.lookup from map
  toId <- Map.lookup to map
  return $ formatEdge fromId toId

-- | Transform the whole graph
transformer :: G.Graph -> OutString
transformer g =
  let
    ls = G.toList g
    (idsOut, idMap) = defineVerticies $ map fst ls
    lss = concatMap (\(n, ds) -> zip (repeat n) ds) ls
  in "flowchart TD;\n" ++ foldl (++) idsOut (map (uncurry $ addTransition idMap) lss)

-- | Writer that outputs a MermaidJs format
mermaidWriter :: Writer
mermaidWriter = MkWriter ( flip writeFile . transformer )

{-$
>>> transformer $ G.addVertex G.empty "v"
"flowchart TD;\n\tid0(\"v\");\n"

>>> transformer $ G.addDependecies G.empty "x" ["y"]
"flowchart TD;\n\tid0(\"x\");\n\tid1(\"y\");\n\tid0 --> id1;\n"

-}
