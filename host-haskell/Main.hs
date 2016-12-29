module Main where
import Data.List (isPrefixOf, intercalate)
import Control.Concurrent (threadDelay)
import System.IO
import Text.Printf

cpuTimesFieldNames :: [String]
cpuTimesFieldNames = ["user", "nice", "system", "idle", "iowait",
                      "irq", "softirq", "steal", "guest", "guest_nice"]

getCpuTimes :: IO [[Integer]]
getCpuTimes = do s <- readFile "/proc/stat"
                 return $! parceCpuTimes s
  where
    parceCpuTimes :: String -> [[Integer]]
    parceCpuTimes = map (map read . tail . words) . takeWhile (isPrefixOf "cpu") . lines

cpuTimeTot :: [Integer] -> Integer
cpuTimeTot = sum . take 8

cpuTimeBusy :: [Integer] -> Integer
cpuTimeBusy ts = sum $ take 3 ts ++ take 3 (drop 5 ts)

castDiv :: Fractional a => Integer -> Integer -> a
castDiv a b = fromInteger a / fromInteger b

cpuPercentLoad :: Fractional a => [Integer] -> a
cpuPercentLoad ts = castDiv bus tot
  where
    tot = cpuTimeTot ts
    bus = cpuTimeBusy ts

fader :: Fractional a => [[Integer]] -> [[Integer]] -> [a]
fader = zipWith (\i f -> cpuPercentLoad $ zipWith (-) f i)

snaz :: RealFloat a => a -> String
snaz x = ad $ show $ castDiv (floor (x * 10)) 10
  where ad xs = replicate (5 - length xs) ' '  ++ xs

loop :: [[Integer]] -> IO ()
loop lastCpuTimes = do
  threadDelay 1000000
  times <- getCpuTimes
  putChar '\r'
  putStr $ intercalate ", " $ map (snaz . (*100)) $ fader times lastCpuTimes
  hFlush stdout
  loop times

main :: IO ()
main = do
  --readFile "/proc/stat" >>= putStr
  times1 <- getCpuTimes
  loop times1
