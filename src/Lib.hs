{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe

import Control.Applicative

import Text.HTML.Scalpel
import Text.StringLike
import Text.Parsec (many1, anyChar, digit, letter, skipMany1, parse)

sourceUrl :: URL
sourceUrl = "https://www.kpfu.ru" 

sourceScrape = scrapeURL sourceUrl
test = getRefs sourceUrl

getRefs :: URL -> IO (Maybe [URL])
getRefs sourceUrl = scrapeURL sourceUrl refs where
    refs = attrs "href" "a"

run = do
    Just refs <- getRefs sourceUrl
    print $ length refs

--test = "<div id=\"emptyidcc\"> <tbody><p>1</p> <tbody><p>2</p></tbody> </tbody></div> <tbody><p>3</p></tbody>"
someFunc :: IO ()
someFunc = putStrLn "someFunc"
