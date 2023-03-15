-- | Main module.
--
-- @since 0.1
module Main (main) where

import Stackage.Runner (runStackageParser)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = runStackageParser
