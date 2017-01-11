module Main where
import           Control.Concurrent (threadDelay)
import           Data.List          (find, genericIndex, isPrefixOf, replicate)
import           Data.Maybe         (fromJust)
import           System.Environment (getArgs)
import           System.IO          (Handle, hFlush, hPutStr, stdout)
import           System.Serial

-- Basic Functions --

mapT :: (a -> b) -> (a, a) -> [b]
mapT f (a, b) = [f a, f b]

byte2Hex :: Integer -> String
byte2Hex n = mapT (genericIndex "0123456789ABCDEF") $ quotRem n 16

float2Hex :: RealFrac a => a -> String
float2Hex = byte2Hex . round . (* 255)

percentByte :: Integer -> Integer -> Integer
percentByte p h = p * 255 `quot` h

percentLedByte :: Integer -> Integer -> Integer
percentLedByte p h = round ((fromInteger p / fromInteger h) ** 2.2 * 255.0) + 1

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
showCpuLoad (ix:is) (fx:fs) = concat ((byte2Hex $ usagePercent ix fx) : zipWith (\i f -> byte2Hex $ usageLedPercent i f) is fs)
  where
    usagePercent :: (Integer, Integer) -> (Integer, Integer) -> Integer
    usagePercent i f = percentByte (fst f - fst i) (snd f - snd i)
    usageLedPercent :: (Integer, Integer) -> (Integer, Integer) -> Integer
    usageLedPercent i f = percentLedByte (fst f - fst i) (snd f - snd i)

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

showUpdate :: [(Integer, Integer)] -> [(Integer, Integer)] -> String -> String
showUpdate times lastTimes memload = 'x' : showCpuLoad times lastTimes ++ memload

triangleWave :: RealFrac a => Integer -> a -> String
triangleWave s f | s == 0 = float2Hex f
                 | s == 1 = "FF"
                 | s == 2 = float2Hex (1 - f)
                 | otherwise = "00"

-- Execution and Control Flow --

pushUpdate :: Handle -> String -> IO ()
pushUpdate ser update = do
  putChar '\r'
  putStr update
  hFlush stdout

  hPutStr ser update
  hFlush ser

testLoop :: RealFrac a => Handle -> Integer -> a -> IO ()
testLoop ser s f = do
  threadDelay 100000
  let (ns,nf) = if (f + 0.1) > 1 then (if s == 4 then 0 else s + 1, f + 0.1 - 1) else (s, f + 0.1)
  let update = ('x':) . concat . replicate 6 $ triangleWave ns nf

  pushUpdate ser update

  testLoop ser ns nf

doTest :: Handle -> IO ()
doTest ser = testLoop ser 0 0

mainLoop :: Handle -> [(Integer, Integer)] -> IO ()
mainLoop ser lastTimes = do
  threadDelay 100000
  times <- getCpuTimes
  memload <- getMemLoad
  let update = showUpdate times lastTimes memload

  pushUpdate ser update

  mainLoop ser times

doMain :: Handle -> IO ()
doMain ser = getCpuTimes >>= mainLoop ser

main :: IO ()
main = do
  (mode:portName:_) <- getArgs

  ser <- openSerial portName B9600 8 One Even Software

  case mode of
    "test" -> doTest ser
    "start" -> doMain ser
    _ -> error (mode ++ " is not a valid mode.")
