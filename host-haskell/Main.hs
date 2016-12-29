module Main where
import           Control.Arrow      ((&&&))
import           Control.Concurrent (threadDelay)
import           Data.List          (intercalate, isPrefixOf, genericIndex)
import           System.IO          (hFlush, stdout)

mapT :: (a -> b) -> (a, a) -> [b]
mapT f (a, b) = [f a, f b]

cpuTimesFieldNames :: [String]
cpuTimesFieldNames = ["user", "nice", "system", "idle", "iowait",
                      "irq", "softirq", "steal", "guest", "guest_nice"]

getCpuTimes :: IO [(Integer, Integer)]
getCpuTimes = do s <- readFile "/proc/stat"
                 return $! parceCpuTimes s
  where
    parceCpuTimes :: String -> [(Integer, Integer)]
    parceCpuTimes = map parceCpuLine . takeWhile (isPrefixOf "cpu") . lines

    parceCpuLine :: String -> (Integer, Integer)
    parceCpuLine = (cpuTimeBusy &&& cpuTimeTot) . map read . tail . words

    cpuTimeTot :: [Integer] -> Integer
    cpuTimeTot = sum . take 8

    cpuTimeBusy :: [Integer] -> Integer
    cpuTimeBusy ts = sum $ take 3 ts ++ take 3 (drop 5 ts)

levelsMessage :: [(Integer, Integer)] -> [(Integer, Integer)] -> String
levelsMessage is fs = 'x' : concat (zipWith (\i f -> byte2Hex $ percentByte i f) is fs)
  where
    percentByte :: (Integer, Integer) -> (Integer, Integer) -> Integer
    percentByte i f = (fst f - fst i) * 255 `quot` (snd f - snd i)

    byte2Hex :: Integer -> String
    byte2Hex n = mapT (genericIndex "0123456789ABCDEF") $ quotRem n 16

mainLoop :: [(Integer, Integer)] -> IO ()
mainLoop lastTimes = do
  threadDelay 100000
  times <- getCpuTimes
  putChar '\r'
  putStr $ levelsMessage times lastTimes
  hFlush stdout
  mainLoop times

main :: IO ()
main = do
  times <- getCpuTimes
  mainLoop times
