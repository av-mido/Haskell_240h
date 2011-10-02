-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where
import System
import Data.Map as M
import Data.List as L
import Data.Function as F
import Data.Ord as Ord

testFn y =
  if (length y) > 0
  then
    print "2" >> (return $ length $ words y)
  else
    return 2

--CONSTANT
maxRowLength = 80
spaceBuffer = 2
data CountsInfo = CountsInfo  {countsSorted :: [(String, Integer)]
                              , maxWordLength :: Integer} deriving Show



countWords :: [String] -> M.Map String Integer
countWords strs =
  foldr addCounts M.empty strs
addCounts :: String -> M.Map String Integer-> M.Map String Integer
addCounts wrd acc =
  M.insertWith' (+) wrd 1 acc
tCounts = countWords ["abc", "abc", "def", "abc", "Def", "Def", "long", "looooooooooooong"]

sortWords :: M.Map String Integer -> [(String, Integer)]
sortWords counts =
    L.sortBy ((flip compare) `on` snd) $ M.toList counts
tSortedList = sortWords tCounts


--TODO -> as a last step, filter out all pairs where count is 0!
-- because we dont want to print empty bars later
--just kidding, this wont happen because all fractions are rounded up
scaleCounts :: [(String, Integer)] -> CountsInfo
scaleCounts xs =
    let maxWordLength = foldl (\acc x -> max acc $ fromIntegral $length $ fst x) 0 xs
        maxCount = snd $ head xs
    in
      CountsInfo
        (L.map (\x -> (fst x, scaleCount (snd x) maxCount maxWordLength)) xs)
        maxWordLength
tScaleCounts = scaleCounts tSortedList

scaleCount :: Integer -> Integer -> Integer -> Integer
scaleCount x maxCount maxWordLen =
    let maxBarLength = maxRowLength - maxWordLen - spaceBuffer
    in
      ceiling $ (fromIntegral maxBarLength)
      * (fromIntegral x / fromIntegral maxCount)
t2 = scaleCount 2 8 10 --2 8 10 --> 17 assuming maxRowLength equals 80
t3 = scaleCount 3 3 3
printChart :: CountsInfo -> IO ()
--printCharts [] = putStrLn "end"
printChart info =
    let xs = countsSorted info
       -- recurs =
       --           do putStr $ fst $ head xs
       --           putStr "   " --change later
        --          putStr $ snd $ head xs
    in
       mapM_ (printOneBar $ maxWordLength info) xs
tPrintChart = printChart tScaleCounts

printOneBar :: Integer -> (String, Integer) -> IO ()
printOneBar maxWrdLen x =
    do putStr $ fst x
       putStr $
         L.genericReplicate (maxWrdLen - (fromIntegral $ L.genericLength $ fst x)) ' '
       putStr "  " --2 spaces extra buffer
       putStrLn $ L.genericReplicate (snd x) '#'

--printSpaces :: Integer -> String -> String
--printSpaces maxWordLen word =
--    replicate () ' '



         --printCharts xs maxWordLength
--tPrintCharts = printCharts tScaleCounts

--mainOld = do x <- getContents
--          print $ head $ words x
--          let file = readFile $ head $ words $ x
--          let zz = file >>= (return . words)
--          (zz >>= (return . head)) >>= putStrLn

main = do x <- getContents
          let file1 = readFile $ head $ words $ x --here file is of type monad IO
          file <- readFile $ head $ words $ x -- here file is of type String
          let wordsInFile = words file --wordsInFile is of type [String]
          printChart $ scaleCounts $ sortWords $ countWords wordsInFile
          --print wordsInFile





