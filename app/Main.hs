module Main where

import Text.Regex.Posix
import Text.HTML.TagSoup
import System.Environment
import Data.List (intercalate)
import Network.Curl

split :: Char -> String -> [String]
split c xs = words . map (\x -> if x == c then ' ' else x) $ xs

matcher :: String -> Tag String -> Bool
matcher regex tag = and [tag ~== TagOpen "a" [],
                         fromAttrib "href" tag =~ regex]

menu :: [(String, String)] -> String
menu xs = unlines $ index 0 xs
  where index _ [] = []
        index n (y:ys) = [(show n) ++ " - " ++ snd y] ++ index (succ n) ys

filterTags :: [Tag String] -> [(String, String)]
filterTags [] = []
filterTags (x:xs)
  | and [matcher bookListRegex x, not $ null fromLink] =
    [(head . split '&' . fromAttrib "href" $ x,
      fromTagText $ head fromLink)] ++ filterTags xs
  | otherwise = filterTags xs
    where fromLink = dropWhile (not . isTagText) xs

openUrl :: String -> IO String
openUrl url = snd <$> curlGetString url [CurlFollowLocation True]

getBooksList :: [String] -> IO ()
getBooksList [] = pure ()
getBooksList args = do
  src <- openUrl (googleBooks ++ intercalate "+" args)
  let bookList = foldl (\acc x -> if fst x `elem` (map fst acc)
                                  then acc
                                  else x:acc) [] $ filterTags $ parseTags src
  if null bookList then
    getBooksList []
  else do
    putStr $ menu bookList
    putStrLn "\nSelect a book: "
    i <- getLine
    getFormat $ fst $ bookList !! (read i :: Int)

getFormat :: String -> IO ()
getFormat url = do
  src <- openUrl url
  let bibtexUrl = fromAttrib "href" . head . filter (matcher bookBibtexRegex) $ parseTags src
  format <- openUrl bibtexUrl
  writeFile "refs.bib" format
  putStrLn "saved to \"refs.bib\"."

main = getArgs >>= getBooksList

bookListRegex = "https://books\\.google\\.[a-z]+\\/books\\?id=.*"
bookBibtexRegex = "https://books\\.google\\.[a-z]+\\/books\\/download\\/.*&output=bibtex"
googleBooks = "https://www.google.com/search?udm=36&q="
