{-# LANGUAGE TypeFamilies #-}

module Model where

import DeltaQ

checkDataOrder :: [(Rational, Rational)] -> [(Rational, Rational)]
checkDataOrder ((p1,x1):(p2,x2):xs) =
  if x1 <= x2
    then (p1,x1) : checkDataOrder ((p2,x2) : xs)
    else error "delays are not monotonically increasing"
checkDataOrder x = x

measuredDQ :: [(Rational, Rational)] -> DQ
measuredDQ delays = choices dataPoints
  where
    extendedData =
      checkDataOrder $
        if head delays == (0, 0) then delays else (0, 0) : delays

    dataPoints =
      [ (p' - p, delayComponent d' d)
      | ((p, d), (p', d')) <- zip extendedData (tail extendedData)
      ]

    delayComponent d1 d2 =
      if d1 == d2 then wait d1 else uniform d1 d2

probList :: [Rational] -> (Rational -> Rational) -> [(Rational, Rational)]
probList l f = map (\x -> (f x, x)) l

makeList :: Rational -> Rational -> [Rational]
makeList step stop = [0.0, step .. stop]

expCDF :: Double -> Rational -> Rational
expCDF r x = toRational $ 1 - exp (-r * fromRational x)

cdfTrunc :: Outcome o => Duration o -> o -> o
cdfTrunc t dq = dq .\/. wait t

activeFraction :: Double
activeFraction = 0.05

nextBlock :: DQ
nextBlock = measuredDQ $ probList (makeList 10 100) $ expCDF activeFraction

nextBlockCoarse :: DQ
nextBlockCoarse = measuredDQ $ probList (makeList 20 100) $ expCDF activeFraction

ct_B1 :: [(Rational, Rational)]
ct_B1 =
  [ (0.0,0.0),(0.1,3.0),(0.2,5.0),(0.3,8.0),(0.4,11.0),(0.5,14.0)
  , (0.6,19.0),(0.7,24.0),(0.8,32.0),(0.9,46.0),(0.9,46.0),(0.91,48.0)
  , (0.92,50.0),(0.93,53.0),(0.94,56.0),(0.95,60.0),(0.96,64.0),(0.97,70.0)
  , (0.98,77.0),(0.99,91.0),(1.0,203.0)
  ]

partBlock :: DQ
partBlock = measuredDQ ct_B1

all_dPP95 :: [(Rational, Rational)]
all_dPP95 = map (\(x,y) -> (y,x/1000))
  [ (770.0,0.0),(810.0,0.04),(850.0,0.30),(910.0,0.67)
  , (950.0,0.76),(1050.0,0.92),(1175.0,0.96),(1829.0,1.00)
  ]

all_dAP95 :: [(Rational, Rational)]
all_dAP95 = map (\(x,y) -> (y,x/1000))
  [ (58.0,0.0),(80.0,0.21),(120.0,0.61),(145.0,0.74)
  , (200.0,0.90),(230.0,0.94),(349.0,1.00)
  ]

blockProp :: DQ
blockProp = measuredDQ all_dPP95

blockAdpt :: DQ
blockAdpt = measuredDQ all_dAP95

txDiffRec :: [(Rational, Rational)]
txDiffRec = [(0.001,1),(0.014,2),(0.243,3),(0.992,4),(1.00,5)]

shortHop, mediumHop, longHop :: DQ
shortHop = wait 0.012
mediumHop = wait 0.069
longHop = wait 0.268

hopShort = 0.012
hopMedium = 0.069
hopLong = 0.268

txPropHop :: Rational -> DQ
txPropHop hop = measuredDQ $ map (\(x,y) -> (x,hop*y)) txDiffRec 

txProp :: DQ
-- more complex CDF
txProp = choices [(1,txPropHop hopShort),(1,txPropHop hopMedium),(1,txPropHop hopLong)] 
-- less complex CDF
-- txProp = measuredDQ $ map (\(x,y) -> (x, hopMedium * y)) txDiffRec

waitBlock :: Int -> DQ
-- more complex CDF
waitBlock n = foldr (.>>.) (wait 0.0) (replicate n nextBlock)
-- less complex CDF
-- waitBlock n = foldr (.>>.) (wait 0.0) (replicate n nextBlockCoarse)

memPool :: Int -> DQ
memPool section = waitBlock section 

txHandling :: [Rational] -> DQ
txHandling pSections = choices [ (x, memPool i) | (i, x) <- zip [0..] pSections ]

txToAdopt :: [Rational] -> DQ
txToAdopt pSections = (txProp ./\. partBlock) .>>. txHandling pSections .>>. blockProp .>>. blockAdpt

-- includes transaction acceptance or rejection

txProgression :: (Rational, [Rational]) -> DQ
txProgression (pBlock, pSections) = choice pBlock never (txToAdopt pSections)

    -- truncates time to make x-axis shorter

txProgressionTrunc210 :: (Rational, [Rational]) -> DQ
txProgressionTrunc210 (pBlock, pSections) = cdfTrunc 210 $ txProgression (pBlock, pSections)

-- assumes transaction accepted
        
txAccepted :: (Rational,[Rational]) -> DQ
txAccepted probsPair = txToAdopt $ snd probsPair

  -- truncates time to make x-axis shorter

txAcceptedTrunc210 :: (Rational, [Rational]) -> DQ
txAcceptedTrunc210 probsPair = cdfTrunc 210 $ txAccepted probsPair 
