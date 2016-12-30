module Main where
import           Control.Concurrent (threadDelay)
import           Data.List          (find, genericIndex, isPrefixOf)
import           Data.Maybe         (fromJust)
import           System.IO          (hFlush, stdout)

-- Basic Functions --

mapT :: (a -> b) -> (a, a) -> [b]
mapT f (a, b) = [f a, f b]

byte2Hex :: Integer -> String
byte2Hex n = mapT (genericIndex "0123456789ABCDEF") $ quotRem n 16

percentByte :: Integer -> Integer -> Integer
percentByte p h = p * 255 `quot` h

-- Data Fetching --

getCpuTimes :: IO [(Integer, Integer)]
getCpuTimes = do s <- readFile "/proc/stat"
                 return $! parceCpuTimes s
  where
    parceCpuTimes :: String -> [(Integer, Integer)]
    parceCpuTimes = map parceCpuLine . takeWhile (isPrefixOf "cpu") . lines

    parceCpuLine :: String -> (Integer, Integer)
    parceCpuLine l = (b, t)
      where
        times = map read . tail $ words l
        t = sum $ take 8 times
        b = sum $ take 3 times ++ take 3 (drop 5 times)

showCpuLoad :: [(Integer, Integer)] -> [(Integer, Integer)] -> String
showCpuLoad is fs = concat (zipWith (\i f -> byte2Hex $ usagePercent i f) is fs)
  where
    usagePercent :: (Integer, Integer) -> (Integer, Integer) -> Integer
    usagePercent i f = percentByte (fst f - fst i) (snd f - snd i)

getMemLoad :: IO String
getMemLoad = do s <- readFile "/proc/meminfo"
                return $ parceMeminfo s
  where
    parceMeminfo :: String -> String
    parceMeminfo s = byte2Hex $ usagePercent (a, t)
      where
        ls = lines s
        a = parceMemLine $ find (isPrefixOf "MemAvailable") ls
        t = parceMemLine $ find (isPrefixOf "MemTotal") ls

    parceMemLine :: Maybe String -> Integer
    parceMemLine l = read $ (!! 1) $ words $ fromJust l

    usagePercent :: (Integer, Integer) -> Integer
    usagePercent (a, t) = percentByte (t - a) t

-- Execution and Control Flow --

mainLoop :: [(Integer, Integer)] -> IO ()
mainLoop lastTimes = do
  threadDelay 200000
  times <- getCpuTimes
  memload <- getMemLoad
  putChar '\r'
  putChar 'x'
  putStr $ showCpuLoad times lastTimes
  putStr memload
  hFlush stdout
  mainLoop times

main :: IO ()
main = do
  times <- getCpuTimes
  mainLoop times
