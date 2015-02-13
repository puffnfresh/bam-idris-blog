module Main

import Config.JSON
import Control.Category
import Data.SortedMap
import Effects
import Lens
import Network.Cgi
import System
import System.Posix.Directory
import System.Posix.Time

import JSONLenses
import Template

loadArticleContent : String -> IO String
loadArticleContent s = readFile ("articles/" ++ s ++ ".md")

-- TODO: Implement
loadArticle : String -> IO (Maybe Article)
loadArticle s = do
  putStrLn $ "Loading article " ++ s
  json <- runInit [(), ()] (readJSONConfig $ "articles/" ++ s ++ ".json")
  let title = getPL (jsonStringPL . jsonKeyPL "title") json
  let author = getPL (jsonStringPL . jsonKeyPL "author") json
  let date = getPL (jsonStringPL . jsonKeyPL "date") json
  let date = getPL (jsonStringPL . jsonKeyPL "date") json
  let hidden = fromMaybe False $ getPL (jsonBoolPL . jsonKeyPL "hidden") json
  if hidden
  then return Nothing
  else do
    content <- loadArticleContent s
    return [| MkArticle (pure s) title author date (pure content) |]

loadArticleList : IO (List Article)
loadArticleList = do
  contents <- getDirectoryContents "articles"

  -- Awful!
  let suffix = ".json"
  let suffixLength = length suffix
  let jsonFiles = filter (isSuffixOf suffix) contents
  let names = map (reverse . pack . drop suffixLength . unpack . reverse) jsonFiles
  articles <- traverse loadArticle names

  return . reverse . sort $ catMaybes articles

writeFile : String -> String -> IO ()
writeFile f s = do
  putStrLn $ "Writing " ++ f
  f <- openFile f Write
  fwrite f s
  closeFile f

dumpWrapped : String -> String -> String -> IO ()
dumpWrapped f t s = writeFile f (wrap t s)

main : IO ()
main = do
  articles <- loadArticleList

  traverse (\a => dumpWrapped ("generated/" ++ articleSlug a ++ ".htm") (articleName a ++ " - BAM Weblog") (renderArticle a)) articles

  dumpWrapped "generated/index.htm" "BAM Weblog" (renderIndex articles)

  t <- System.Posix.Time.time
  ts <- strftime "%Y-%m-%d" t
  writeFile "generated/feed.xml" (feedWrap ts (renderFeedIndex articles))
