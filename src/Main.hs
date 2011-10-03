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
import Data.Char as Char

--CONSTANT
maxRowLength = 80
spaceBuffer = 2
data CountsInfo = CountsInfo  {countsSorted :: [(String, Integer)]
                              , maxWordLength :: Integer} deriving Show

toLowerCase :: [String] -> [String]
toLowerCase = L.map (L.map Char.toLower)

removeOuterPunctuation :: String -> String
removeOuterPunctuation = reverse .
                          dropWhile (not . isAlpha) . reverse .
                          dropWhile (not . isAlpha)
tRem2 = removeOuterPunctuation "--don't++"



countWords :: [String] -> M.Map String Integer
countWords =
  foldl' (\acc wrd -> M.insertWith' (+) wrd 1 acc) M.empty
tCounts = countWords ["abc", "abc", "def", "abc", "Def", "Def", "long", "looooooooooooong"]

sortWords :: M.Map String Integer -> [(String, Integer)]
sortWords counts =
    L.sortBy (flip compare `on` snd) $ M.toList counts
tSortedList = sortWords tCounts

scaleCounts :: [(String, Integer)] -> CountsInfo
scaleCounts xs =
    let maxWordLength = foldl' (\acc x -> max acc $ fromIntegral $length $ fst x) 0 xs
        maxCount = snd $ head xs
    in
      CountsInfo
        (L.map (\x -> (fst x, scaleCount (snd x) maxCount maxWordLength)) xs)
        maxWordLength
tScaleCounts = scaleCounts tSortedList

--not sure how to avoid using fromIntegral...
scaleCount :: Integer -> Integer -> Integer -> Integer
scaleCount x maxCount maxWordLen =
    let maxBarLength = maxRowLength - maxWordLen - spaceBuffer
    in
      ceiling $ fromIntegral maxBarLength
      * (fromIntegral x / fromIntegral maxCount)

printChart :: CountsInfo -> IO ()
printChart info =
    let xs = countsSorted info
    in
       mapM_ (printOneBar $ maxWordLength info) xs
tPrintChart = printChart tScaleCounts

printOneBar :: Integer -> (String, Integer) -> IO ()
printOneBar maxWrdLen x =
    do putStr $ fst x
       putStr $
         L.genericReplicate (maxWrdLen - fromIntegral (L.genericLength $ fst x)) ' '
       putStr "  " --2 spaces extra buffer
       putStrLn $ L.genericReplicate (snd x) '#'

main = do
          args <- getArgs
          contents <- if 0 == length args
                      then
                        getContents
                      else
                        readFile $ head args
          let wordsInFile = words contents
          --todo remove empty strings
          let cleanedWords = L.map removeOuterPunctuation $ toLowerCase wordsInFile
          let cleanedWords2 = L.filter (not . L.null) cleanedWords
          printChart $ scaleCounts $ sortWords $ countWords cleanedWords2
          --print $ sortWords $ countWords cleanedWords2





