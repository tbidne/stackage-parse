-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (displayException)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import Stackage.Runner (runStackageParser)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  runStackageParser
