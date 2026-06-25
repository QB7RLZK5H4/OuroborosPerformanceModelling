-- Usage:
--
-- In the definition of the variable chart below, choose 
-- an appropriate plot variable as parameters
-- (Optional: make the output file name more meaningful)
--
-- Command line execution: "cabal build" then "cabal run"
--
-- Note that these plots truncate at 210 seconds so that
-- the interesting parts of the plots are easier to see.
-- To change this, replace txProgressionTrunc210 with
-- txProgression and txAcceptedTrunc210 with txAccepted.
-- (Optional: define your own trunacation in Model.hs)
--
--
-- Warning:
--
-- The current model.hs has a very long running time 
-- (around 8 hours on an Macbook Air M4 with 24GB memeroy and 
-- 100GB free disk space for swap and very few other processes
--
-- For faster execution (with slightly less accuracy),
-- make the following change to Model.hs:
--
-- Search for waitBlock and comment out the more complex CDF,
-- and uncomment the less complex CDF (this reduces the CDF for
-- an exponential distribution from 11 points to 6 points).
--
-- (Optional: a further simplication is to make a similar change 
-- for txProp, but this is less ideal)


{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import BSM     -- bulk service model data
import Model   -- other data and function for the Delta model
import DeltaQ

import Data.Maybe (fromMaybe)
import "Chart" Graphics.Rendering.Chart.Layout (layoutToRenderable)
import "Chart-cairo" Graphics.Rendering.Chart.Backend.Cairo

myOptions :: FileOptions
myOptions = FileOptions (600,400) PDF

-- set up data from bulk service model results

low_p2 = fromMaybe (-1.0,[-1.0]) $ lookup 0.1 bulkServiceResultsCapK2pN176 
low_p5 = fromMaybe (-1.0,[-1.0]) $ lookup 0.1 bulkServiceResultsCapK5pN176
low_p8 = fromMaybe (-1.0,[-1.0]) $ lookup 0.1 bulkServiceResultsCapK8pN176

med_p2 = fromMaybe (-1.0,[-1.0]) $ lookup 0.5 bulkServiceResultsCapK2pN176 
med_p5 = fromMaybe (-1.0,[-1.0]) $ lookup 0.5 bulkServiceResultsCapK5pN176
med_p8 = fromMaybe (-1.0,[-1.0]) $ lookup 0.5 bulkServiceResultsCapK8pN176

hgh_p2 = fromMaybe (-1.0,[-1.0]) $ lookup 0.9 bulkServiceResultsCapK2pN176
hgh_p5 = fromMaybe (-1.0,[-1.0]) $ lookup 0.9 bulkServiceResultsCapK5pN176
hgh_p8 = fromMaybe (-1.0,[-1.0]) $ lookup 0.9 bulkServiceResultsCapK8pN176

-- set up plot options

title = "Varying mempool/blocksize ratios: "

low_progression = (title ++ "low load",
    [ ("low: 2x", txProgressionTrunc210 low_p2)
    , ("low: 5x", txProgressionTrunc210 low_p5)
    , ("low: 8x", txProgressionTrunc210 low_p8)
    ])

med_progression = (title ++ "medium load",
    [ ("med: 2x", txProgressionTrunc210 med_p2)
    , ("med: 5x", txProgressionTrunc210 med_p5)
    , ("med: 8x", txProgressionTrunc210 med_p8)
    ])

hgh_progression = (title ++ "high load",
    [ ("hgh: 2x", txProgressionTrunc210 hgh_p2)
    , ("hgh: 5x", txProgressionTrunc210 hgh_p5)
    , ("hgh: 8x", txProgressionTrunc210 hgh_p8)
    ])

low_accepted = (title ++ "low load",
    [ ("low: 2x", txAcceptedTrunc210 low_p2)
    , ("low: 5x", txAcceptedTrunc210 low_p5)
    , ("low: 8x", txAcceptedTrunc210 low_p8)
    ])

med_accepted = (title ++ "medium load",
    [ ("med: 2x", txAcceptedTrunc210 med_p2)
    , ("med: 5x", txAcceptedTrunc210 med_p5)
    , ("med: 8x", txAcceptedTrunc210 med_p8)
    ])

hgh_accepted = (title ++ "high load",
    [ ("hgh: 2x", txAcceptedTrunc210 hgh_p2)
    , ("hgh: 5x", txAcceptedTrunc210 hgh_p5)
    , ("hgh: 8x", txAcceptedTrunc210 hgh_p8)
    ])

chart params = plotCDFs (fst params) (snd params)

-- replace argument as required
plot = chart med_progression

main :: IO ()
main =
  renderableToFile myOptions "plot.pdf" (layoutToRenderable plot) >> pure ()

