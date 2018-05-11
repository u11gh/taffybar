-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.CPU2
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about used CPU times, obtained from parsing the
-- @\/proc\/stat@ file using some of the facilities included in the
-- "System.Taffybar.Information.StreamInfo" module.
-- And also provides information about the temperature of cores.
-- (Now supports only physical cpu).
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.CPU2 ( getCPULoad, getCPUInfo, getCPUTemp ) where

import Data.Maybe ( mapMaybe )
import Safe ( atMay, readDef, tailSafe )

import System.Taffybar.Information.StreamInfo ( getLoad, getParsedInfo )
import Control.Monad (liftM)
import System.Directory (doesDirectoryExist)

-- | Returns a two-element list containing relative system and user times
-- calculated using two almost simultaneous samples of the @\/proc\/stat@ file
-- for the given core (or all of them aggregated, if \"cpu\" is passed).
getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
  load <- getLoad 0.05 $ getCPUInfo cpu
  case load of
    l0:l1:l2:_ -> return [ l0 + l1, l2 ]
    _ -> return []

-- | Returns a list containing temperatures of user given cpu cores.
-- Use ["cpu1", "cpu2".."cpuN"] to get temperature of exact cores.
-- Use ["cpu0"] to get common temperature.
getCPUTemp :: [String] -> IO [Int]
getCPUTemp cpus = do
    let cpus' = map (\s -> drop 3 s) cpus
        baseDir = "/sys/bus/platform/devices/coretemp.0"
        getTemps dir = liftM concat $ mapM (\cpu -> getParsedInfo (baseDir ++ dir ++ "/temp" ++ show (read cpu + 1) ++ "_input") (\s -> [("temp", [read s `div` 1000])]) "temp") cpus'
    newDirectory <- doesDirectoryExist $ baseDir ++ "/hwmon/hwmon0"
    getTemps $ if newDirectory then "/hwmon/hwmon0" else ""
    --TODO and suppoprt for more than 1 physical cpu.

-- | Returns a list of 5 to 7 elements containing all the values available for
-- the given core (or all of them aggregated, if "cpu" is passed).
getCPUInfo :: String -> IO [Int]
getCPUInfo = getParsedInfo "/proc/stat" parse

parse :: String -> [(String, [Int])]
parse = mapMaybe (tuplize . words) . filter (\x -> take 3 x == "cpu") . lines

tuplize :: [String] -> Maybe (String, [Int])
tuplize s = do
  cpu <- s `atMay` 0
  return (cpu, map (readDef (-1)) (tailSafe s))
