module Main (main) where
import           Control.Concurrent (threadDelay)
import           Data.List          (find, genericIndex, isPrefixOf, replicate)
import           Data.Maybe         (fromJust)
import           System.Environment (getArgs)
import           System.IO          (Handle, hFlush, hPutStr, stdout, openFile, IOMode(WriteMode))
import           System.Serial

type Times = (Integer, Integer)

-- Basic Functions --

tupleSubtract (a,b) (c,d) = (a - c, b - d)

byte2Hex :: Integer -> String
byte2Hex n = mapT (genericIndex "0123456789ABCDEF") $ quotRem n 16
  where mapT f (a, b) = [f a, f b]

float2Times :: RealFrac a => a -> Times
float2Times x = (round (x * 1024), 1024)

percent2Hex :: Times -> String
percent2Hex t = byte2Hex $ fst t * 255 `quot` snd t

percent2GammaHex :: Times -> String
percent2GammaHex t = byte2Hex $ round ((fromInteger (fst t) / fromInteger (snd t)) ** 2.2 * 255.0) + 1

-- Data Fetching --

getCpuTimes :: IO [Times]
getCpuTimes = do s <- readFile "/proc/stat"
                 return $! parce s
  where
    parce = map parce' . takeWhile (isPrefixOf "cpu") . lines
    parce' l = (b, t)
      where
        times = map read . tail $ words l
        t = sum $ take 8 times
        b = sum $ take 3 times ++ take 3 (drop 5 times)

getMemLoad :: IO Times
getMemLoad = do s <- readFile "/proc/meminfo"
                return $! parse s
  where
    parse s = (t - a, t)
      where ls = lines s
            a = parce' "MemAvailable" ls
            t = parce' "MemTotal" ls
    parce' a = read . (!! 1) . words . fromJust . find (isPrefixOf a)

triangleWave :: (RealFrac a) => a -> a
triangleWave x | mx < 1 = mx
               | mx < 2 = 1
               | mx < 3 = 3 - mx
               | otherwise = 0
              where mx = x `fmod` 4
                    fmod a b | a < b = a
                             | otherwise = fmod (a-b) b

-- Execution and Control Flow --

pushUpdate :: Handle -> [Times] -> IO ()
pushUpdate ser update = do
  putStr $ '\r':s
  hFlush stdout

  hPutStr ser s
  hFlush ser
    where
      s = 'x' : concat (zipWith ($) (m:m:repeat l) update)
      m = percent2Hex
      l = percent2GammaHex


doTest :: Handle -> IO ()
doTest ser = testLoop ser 0

testLoop :: (RealFrac a) => Handle -> a -> IO ()
testLoop ser t = do
  threadDelay 100000
  let nt = t + 0.1
  let update = replicate 6 . float2Times $ triangleWave nt

  pushUpdate ser update

  testLoop ser nt


doMain :: Handle -> IO ()
doMain ser = getCpuTimes >>= mainLoop ser

mainLoop :: Handle -> [Times] -> IO ()
mainLoop ser lastTimes = do
  threadDelay 100000
  times <- getCpuTimes
  memload <- getMemLoad
  let update = memload : zipWith tupleSubtract times lastTimes

  pushUpdate ser update

  mainLoop ser times


main :: IO ()
main = do
  (mode:portName:_) <- getArgs

  --ser <- openSerial portName B9600 8 One Even Software
  ser <- openFile "/dev/null" WriteMode

  case mode of
    "test" -> doTest ser
    "start" -> doMain ser
    _ -> error (mode ++ " is not a valid mode.")
