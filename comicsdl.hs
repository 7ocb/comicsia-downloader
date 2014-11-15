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

import Debug.Trace
import Control.Monad.Reader
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Applicative
import qualified System.FilePath as P
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Console.GetOpt

import Safe (headMay)

import qualified Data.Text as Text

import Data.XML.Types (Name)
import Text.HTML.DOM (parseLBS)
import qualified Data.ByteString.Lazy as L

data Page = Page { stripImageUrl :: String,
                   stripNumber :: Int,
                   stripTitle :: String,
                   nextPageUrl :: Maybe String,
                   prevPageUrl :: Maybe String
                 } deriving (Show)


pageFromUrl url = parsePage <$> simpleHttp url

parsePage :: L.ByteString -> Maybe Page
parsePage contents = Page <$> url <*> number <*> title <*> link "NextLink" <*> link "PrevLink"

    where page = fromDocument $ parseLBS contents
          get getter axis = headMay $ Text.unpack `fmap` ((page $// axis) >>= getter)

          url       = get (attribute "src") (element "img" >=> hasAttribute "id")
          link name = Just $ ("http://comicsia.ru" ++) <$> get (attribute "href") (element "a" >=> attributeIs "id" name)
          number    = read <$> get ($// content) (element "div" >=> attributeIs "id" "strip-number")
          title     = get ($// content) (element "title")

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

saveComics page = liftIO $ do
  let fileName = (imageName page)

  simpleHttp (stripImageUrl page) >>= L.writeFile fileName

  putStrLn $ "Wrote " ++ fileName

maybeM = maybe (return())           
           
goDownload nextPage page =
  case (nextPage page) of
    Just url -> pageFromUrl url >>= maybeM process

    Nothing -> return ()

  where process next = do
          cfg <- ask
          if inRange cfg next
          then do saveComics next
                  goDownload nextPage next
          else return ()

inRange :: Cfg -> Page -> Bool
inRange cfg p = not $ (aboveRange cfg p) || (underRange cfg p)

aboveRange :: Cfg -> Page -> Bool
aboveRange cfg p = maybe False ((stripNumber p) >) $ cfgLimit cfg

underRange :: Cfg -> Page -> Bool
underRange cfg p =  maybe False ((stripNumber p) <) $ cfgFrom cfg

data Opt = Start String
         | Limit String
         | Help
           deriving (Show, Eq)

arguments = [ (Option ['f'] ["first"] (ReqArg Start "FIRST") "Number of strip to start from")
            , (Option ['l'] ["last"] (ReqArg Limit "LAST") "Number of strip to end on, inclusive")
            , (Option ['h'] ["help"]  (NoArg Help) "Show this help")
            ]

data Cfg = Cfg {
      cfgFrom :: Maybe Int,
      cfgLimit :: Maybe Int
    } deriving (Show, Eq)

parseArgs :: [String] -> Except String (Cfg, String)
parseArgs args = do
  let (opts, urls, errors) = getOpt Permute arguments args
      guard cond err = when cond $ throwError err

      asInt :: String -> Except String Int
      asInt s = do let r = reads s :: [(Int, String)]
                   guard (null r) $ "Can't parse " ++ s ++ " as int"
                   let ((i,_):_) = r
                   return i

      process :: Cfg -> Opt -> Except String Cfg
      process c (Start s) = asInt s >>= \i -> return $ c { cfgFrom = Just i }
      process c (Limit s) = asInt s >>= \i -> return $ c { cfgLimit = Just i }

  guard (Help `elem` opts) $ usageInfo "comicsdl [opts] url" arguments
  guard (not $ null errors) (head errors)
  guard (null urls) "No urls provided"

  cfg <- foldM process (Cfg Nothing Nothing) opts

  guard ((Just True) == ((>) <$> (cfgFrom cfg) <*> (cfgLimit cfg))) "From can't be after limit"

  return (cfg, head urls)

type App = ReaderT Cfg IO

app :: App ()
app = liftIO $ putStrLn "app"

locatePage :: String -> App ()
locatePage url = do
  p <- pageFromUrl url
  cfg <- ask

  case p of
    Just page -> do
           let go next = do
                   liftIO $ putStrLn $ "skipping strip: " ++ (show $ stripNumber page)
                   maybeM locatePage $ next page
               goDl next = goDownload next page



           if inRange cfg page
           then do saveComics page
                   goDl nextPageUrl
                   goDl prevPageUrl
           else if underRange cfg page
                then go nextPageUrl
                else go prevPageUrl

    Nothing -> return ()

main = do
  args <- (runExcept . parseArgs) <$> getArgs

  case args of
    Left error -> putStrLn error

    Right (cfg, url) -> runReaderT (locatePage url) cfg
