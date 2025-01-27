module Main where

import Text.Regex.Posix
import Text.HTML.TagSoup
import System.Environment
import Data.List (intercalate)
import Network.Curl

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

split :: Char -> String -> [String]
split c xs = words . map (\x -> if x == c
                           then
                             ' '
                           else
                             x
                         ) $ xs

matchBook :: Tag String -> Bool
matchBook tag = and [tag ~== TagOpen "a" [],
                      fromAttrib "href" tag =~ "https://books\\.google\\.[a-z]+\\/books\\?id=.*"]

matchBibtex :: Tag String -> Bool
matchBibtex tag = and [tag ~== TagOpen "a" [],
                       fromAttrib "href" tag =~ "https://books\\.google\\.[a-z]+\\/books\\/download\\/.*&output=bibtex"]

menu :: [String] -> String
menu xs = unlines $ helper 0 xs
  where
    helper _ [] = []
    helper n (y:ys) = [(show n) ++ " - " ++ y] ++ helper (succ n) ys

getFormat :: String -> IO ()
getFormat url = do
  src <- curlGetString url [CurlFollowLocation True]
  let bibtexUrl = fromAttrib "href" . head . filter matchBibtex $ parseTags $ snd src
  format <- curlGetString bibtexUrl [CurlFollowLocation True]
  writeFile "refs.bib" (snd format)
  putStrLn "saved to \"refs.bib\"."

getBookList :: [String] -> IO ()
getBookList args = do
  src <- curlGetString (googleBooks ++ intercalate "+" args) [CurlFollowLocation True]
  let bookList = nub . map (head . split '&' . fromAttrib "href") $ filter matchBook (parseTags $ snd src)

  putStrLn "Select a book:"
  putStr $ menu bookList
  i <- getLine
  
  getFormat $ bookList !! (read i :: Int)

main = getArgs >>= getBookList

googleBooks = "https://www.google.com/search?udm=36&q="
