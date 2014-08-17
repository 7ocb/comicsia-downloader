{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Text.XML.Cursor (fromDocument,
                        element,
                        content,
                        attribute,
                        hasAttribute,
                        attributeIs,
                        (>=>),
                        ($//),
                        Cursor,
                        Axis)

import qualified System.FilePath as P
import Text.Printf (printf)
import System.Environment (getArgs)    

import Control.Monad (liftM, liftM3)
    
import qualified Data.Text as Text

import Data.XML.Types (Name)
import Text.HTML.DOM (parseLBS)
import qualified Data.ByteString.Lazy as L

maybeHead (x:_) = Just x
maybeHead []  = Nothing

get :: (Cursor -> [Text.Text]) -> Axis -> Cursor -> Maybe String
get getter axis cursor = maybeHead $ do
  element <- cursor $// axis
  (Text.unpack) `fmap` (getter element)

getComicsImageUrl = get (attribute "src")  (element "img" >=> hasAttribute "id")
getNextLinkUrl    = get (attribute "href") (element "a" >=> attributeIs "id" "NextLink")
getStripNumber    = get ($// content)      (element "div" >=> attributeIs "id" "strip-number")
getStripTitle     = get ($// content)      (element "title")

data Page = Page { stripImageUrl :: String,
                   stripNumber :: Int,
                   stripTitle :: String,
                   nextPageUrl :: Maybe String
                 } deriving (Show)

pageFromUrl :: String -> IO (Maybe Page)
pageFromUrl url = do
  page <- (fromDocument . parseLBS) `fmap` (simpleHttp url)

  return $ (\x -> x $ getNextLinkUrl page) `fmap` (liftM3 Page
                                                   (getComicsImageUrl page)
                                                   (read `fmap` (getStripNumber page))
                                                   (getStripTitle page))

saveImage url name = simpleHttp url >>= L.writeFile name

imageName page =
    printf "%04d - %s%s" (stripNumber page) safeName extension
    where extension = P.takeExtension $ stripImageUrl page
          fullName = filter isSafe $ stripTitle page

          safeName = if length fullName > maxNameLen
                     then (take (maxNameLen - 3) fullName) ++ "..."
                     else fullName

          isSafe = not . (`elem` charactersBadForFs)
          charactersBadForFs = "/\\"
          maxNameLen = 120
    
                     
downloadComics url = pageFromUrl url >>= guarded processPage
    where guarded action maybeValue = case maybeValue of
                                   Just value -> action value
                                   Nothing -> return ()
                                              
          processPage  page = do
            let fileName = (imageName page)
                           
            saveImage (stripImageUrl page) fileName

            putStrLn $ "Wrote " ++ fileName
            
            guarded downloadComics $ ("http://comicsia.ru" ++ ) `fmap` (nextPageUrl page)


                 
main = do
  args <- getArgs

  if null args
  then putStrLn "Usage: comicsdl <url-to-start-from>"
  else downloadComics $ head args 
