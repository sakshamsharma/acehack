--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.Monoid (mappend)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Hakyll
import           Hakyll.Web.Tags
import           System.FilePath.Posix  (takeBaseName, takeDirectory,
                                         (</>), takeFileName)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

       match "assets/js/**" $ do
             route assetsRoute
             compile copyFileCompiler

       match "assets/css/**" $ do
             route assetsRoute
             compile compressCssCompiler

       match "assets/images/**" $ do
             route assetsRoute
             compile copyFileCompiler

       match "assets/fonts/**" $ do
             route assetsRoute
             compile copyFileCompiler

       match (fromList ["404.md", "CNAME"]) $ do
             route idRoute
             compile copyFileCompiler

       tags <- buildTags "posts/**" (fromCapture "tags/*.html")

       let posts = recentFirst =<< loadAll "posts/**"
       let postCtx = dateField "date" "%B %e, %Y" `mappend`
             tagsField "tagsCtx" tags `mappend`
             defaultContext

       match "posts/**" $ do
             route $ postRoute
             compile $ do
               pandocCompiler
                     >>= loadAndApplyTemplate "templates/with-sidebar.html" postCtx
                     >>= loadAndApplyTemplate "templates/default.html"      postCtx
                     >>= relativizeUrls

       match "templates/**" $ compile templateBodyCompiler

       create ["index.html"] $ do
         route idRoute
         let title = "AceHack"
         compile $ do
           let indexCtx =
                 listField "posts" postCtx posts `mappend`
                 constField "title" title        `mappend`
                 defaultContext

           makeItem ""
             >>= loadAndApplyTemplate "templates/index.html"        indexCtx
             >>= loadAndApplyTemplate "templates/with-sidebar.html" indexCtx
             >>= loadAndApplyTemplate "templates/default.html"      indexCtx
             >>= relativizeUrls
             >>= cleanIndexHtmls

       create ["archives.html"] $ do
         route $ cleanRoute True
         let title = "Archive"
         compile $ do
           let archiveCtx =
                 listField "posts" postCtx posts `mappend`
                 constField "title" title        `mappend`
                 defaultContext

           makeItem ""
             >>= loadAndApplyTemplate "templates/archive.html"      archiveCtx
             >>= loadAndApplyTemplate "templates/with-sidebar.html" archiveCtx
             >>= loadAndApplyTemplate "templates/default.html"      archiveCtx
             >>= relativizeUrls

       match (fromList ["about.md"])$ do
         route $ cleanRoute True
         compile $ do
           pandocCompiler
             >>= loadAndApplyTemplate "templates/with-sidebar.html" defaultContext
             >>= loadAndApplyTemplate "templates/default.html"      defaultContext
             >>= relativizeUrls

       create ["sitemap.xml"] $ do
              route   idRoute
              compile $ do
                let sitemapCtx =
                        listField "posts" postCtx posts   `mappend`
                        defaultContext
                makeItem ""
                 >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                 >>= cleanIndexHtmls


--------------------------------------------------------------------------------
type Year = String

postsByYear :: Year -> Compiler [Item String]
postsByYear year = do
  posts <- recentFirst =<< loadAll (fromGlob $ "posts/" ++ year ++ "**")
  return posts

buildYears :: MonadMetadata m => Pattern -> m [(Year, Int)]
buildYears pattern = do
    ids <- getMatches pattern
    return . frequency . (map getYear) $ ids
  where
    frequency xs =  M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

getYear :: Identifier -> Year
getYear = takeBaseName . takeDirectory . toFilePath

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"

replacement :: String -> String
replacement = const "/"

pathToPostRoute :: Identifier -> String
pathToPostRoute path =
  year ++ "/" ++ month ++ "/" ++ rest
  where
    year = takeWhile (/= '-') $ fileName
    month = takeWhile (/= '-') . drop 1 . dropWhile (/= '-') $ fileName
    rest = dropWhile (\x -> isDigit x || x == '-') $ fileName
    fileName = drop 1 . dropWhile (/= '/') $ toFilePath path

postRoute :: Routes
postRoute = (customRoute $ pathToPostRoute) `composeRoutes` cleanRoute False

cleanRoute :: Bool -> Routes
cleanRoute isTopLevel =
  customRoute $
  (++ "/index.html") . takeWhile (/= '.') . (adjustPath isTopLevel) . toFilePath
  where
    adjustPath False = id
    adjustPath True = reverse . takeWhile (/= '/') . reverse

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath
