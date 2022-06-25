module Depender.Writer.Default where
import Depender.Writer ( Writer )
import Depender.Writer.Mermaid (mermaidWriter)
import Depender.Writer.Stdout (stdoutWriter)
import Depender.Writer.GraphViz (graphVizWriter)

-- | Default writer list
defaultList :: [(String, Writer)]
defaultList = [("mermaid", mermaidWriter), ("stdout", stdoutWriter), ("Dot", graphVizWriter)]
