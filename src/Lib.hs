{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

--import Data.Maybe
import Data.Matrix
import Data.List (isInfixOf, isPrefixOf)

import Math.LinearAlgebra.Sparse
import Control.Applicative

import Text.HTML.Scalpel
import Text.StringLike
import Text.Parsec (many1, anyChar, digit, letter, skipMany1, parse)

import Debug.Trace as DT

import Control.Parallel


infixl 4 <$$>
(<$$>) = (fmap . fmap)

data Ref = Ref {ref :: URL, refs :: [URL]} deriving (Show)

kpfFilt ref = not (elem '#' ref)                 &&
              ref /= "https://ex.kpfu.ru"        &&
              length ref > 11                    &&
              (not . isInfixOf "javascript") ref &&
              (not . isInfixOf "doi.org") ref &&
              (not . isInfixOf ".jpg") ref 


maybeToList (Just x) = x
maybeToList Nothing  = []

startUrl = "https://www.kpfu.ru"
startUrl' = "https://en.wikipedia.org/wiki/Haskell_Curry"
prefix' = "https://en.wikipedia.org"
sourceUrl :: URL
sourceUrl = startUrl'
t0 = "http://tat.kpfu.ru/"

sourceScrape = scrapeURL sourceUrl
test = getRefs sourceUrl

--getRefs :: URL -> IO (Maybe [URL])
--getRefs sourceUrl = DT.trace sourceUrl $
--        scrapeURL sourceUrl refs
--            where
--    refs = attrs "href" "a"

getRefs :: URL -> IO (Maybe [URL])
getRefs sourceUrl = DT.trace sourceUrl $
        scrapeURL sourceUrl refs >>= d
            where
    refs = attrs "href" "a"
    d (Just x) = (putStr . (++ " ") . show) (length x) >> (return $ Just x)
    d Nothing = (putStr . (++ " ") . show) (0) >> (return $ Just [])


isRef sourceUrl soughtUrl = DT.trace "isRef" $ (fmap . fmap) (DT.trace "is elem" $ boolToInt . (elem soughtUrl)) (getRefs sourceUrl) where
    boolToInt False = 0
    boolToInt True  = 1

matrixGen zrefs (i, j) = elem sought source where
    Just (Ref _ source)  = lookup i zrefs
    Just (Ref sought _)  = lookup j zrefs
    boolToInt False = 0
    boolToInt True  = 1

norm      = foldr ((+) . (^2)) 0
norming x = fmap (/ norm x) x

toSth mat =
     fromLists                           .
     map (\(xs, norm) -> map (/norm) xs) .
     map (\xs -> (xs, norm xs))          .
     toLists                             $
     mat

n  = 30
n' = 47

runM = m n
runM' = m' n'

zrefs n = do
    Just refs <- getRefs sourceUrl
    listRefs' <- sequenceA $ (fmap getRefs . fmap toRef . take n . filter kpfFilt) refs
    let listRefs = map maybeToList listRefs'
    let zrefs' = zipWith Ref refs listRefs
    let zrefs = zip [1..] zrefs'
    return zrefs

sequential zrefs = do
    let adjMatrix = toSth . fmap boolToDouble . matrix n n $ matrixGen zrefs
    print $ adjMatrix
    let startV = matrix n 1 (\(_,_) -> 0.5)
    let eigVector = getEigvec multStd2 startV (transpose adjMatrix) 0.001
    print (transpose eigVector)
    return (transpose eigVector)

parallel zrefs adjMatrix = do
    let sAdjMatrix = sparseMx . toLists . transpose $ adjMatrix
    let sStartV = sparseList . take n . repeat $ 0.5
    let sEigVector = getEigvec mult' sStartV (sAdjMatrix) 0.001
    print (sEigVector)
    return ()

m n = do
    Just refs <- getRefs sourceUrl
    listRefs' <- sequenceA $ (fmap getRefs . fmap toRef . take n . filter kpfFilt) refs
    let listRefs = map maybeToList listRefs'
    let zrefs' = zipWith Ref refs listRefs
    let zrefs = zip [1..] zrefs'
    --print $ length zrefs

    let adjMatrix = toSth . fmap boolToDouble . matrix n n $ matrixGen zrefs
    --print $ adjMatrix
    let startV = matrix n 1 (\(_,_) -> 0.5)
    let eigVector = getEigvec multStd2 startV (transpose adjMatrix) 0.001

    let sAdjMatrix = sparseMx . toLists . transpose $ adjMatrix
    let sStartV = sparseList . take n . repeat $ 0.5
    let sEigVector = getEigvec mulMV sStartV (sAdjMatrix) 0.001

    writeFile "data/matrix" (show . toList $ adjMatrix)
    writeFile "data/smatrix" (show . toAssocList $ sAdjMatrix)
    writeFile "data/eig" (show . toList $ eigVector)
    writeFile "data/seig" (show . vecToAssocList $ sEigVector)

    return (transpose eigVector, sEigVector)

m' n = do
    Just refs <- getRefs sourceUrl
    listRefs' <- sequenceA $ (fmap getRefs . fmap toRef . take n . filter kpfFilt) refs
    let listRefs = map maybeToList listRefs'
    let zrefs' = zipWith Ref refs listRefs
    let zrefs = zip [1..] zrefs'
    --print $ length zrefs

    let adjMatrix = toSth . fmap boolToDouble . matrix n n $ matrixGen zrefs
    --print $ adjMatrix
    let startV = matrix n 1 (\(_,_) -> 0.5)
    let eigVector = getEigvec multStd2 startV (transpose adjMatrix) 0.001

    let sAdjMatrix = sparseMx . toLists . transpose $ adjMatrix
    let sStartV = sparseList . take n . repeat $ 0.5
    let sEigVector = getEigvec mulMV sStartV (sAdjMatrix) 0.001

    writeFile "data/matrix" (show . toList $ adjMatrix)
    writeFile "data/smatrix" (show . toAssocList $ sAdjMatrix)
    writeFile "data/eig" (show . toList $ eigVector)
    writeFile "data/seig" (show . vecToAssocList $ sEigVector)

    return sEigVector

boolToDouble True  = 1.0
boolToDouble False = 0.0

toRef ref | isPrefixOf "https" ref = ref
          | otherwise              = prefix' ++ ref

toPairs []  = []
toPairs [x] = []
toPairs (x:x0:xs) = (x, x0) : toPairs (x0:xs)

countDiff (x, x0) = (x, dif) where
    dif  = norm $ x - x0

getEigvec matMult startVector mat eps = fst . head            .
                    dropWhile (\(_,dif) -> dif >= eps)        .
                    map countDiff                             .
                    toPairs                                   .
                    iterate (matMult mat) $ startVector
                                where

eyeNGen n (i,j) | i == j = n
                | i /= j = 0

eye3 = matrix 3 3 $ eyeNGen 3

tm = fromLists [[0.3, 0.7, 0.0],
      [0.1, 0.2, 0.7],
      [0.5, 0.3, 0.2]]

tv = fromList 3 1 (norming [0.1,11,0.5])

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parHelp :: ( Num a ) => [a] -> [a] -> a 
parHelp [] [] = 0
parHelp (x : xs ) (y : ys) = ret where 
 ret = par a (pseq b (a + b)) where 
        a = x * y 
        b = parHelp xs ys

helpMult :: (Num a) => [a] -> [[a]] -> [a]
helpMult _ [] = [] 
helpMult x (y:ys) = ret where 
 ret =  par a (pseq b  (a:b)) where 
   a = sum . zipWith (*) x $ y
   b = helpMult x ys

mult :: (Num a) => [[ a ] ] -> [ [ a ] ] -> [ [ a ] ]
mult [] _ = []
mult ( x : xs ) ys = ret where
 ret = par a ( pseq b  ( a : b ) ) where
    a = helpMult x ys
    b = mult xs ys

mult' = mulMV
