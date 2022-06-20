module Depender.Writer where

import Depender.DependencyGraph

-- | A Graph to output type
newtype Writer = MkWriter {runWriter :: Graph -> FilePath -> IO ()}

instance Show Writer where
  show _ = "<writer>"

-- | Return a writer on it's name
getWriter :: [(String, Writer)] -> String -> Maybe Writer
getWriter = flip lookup
