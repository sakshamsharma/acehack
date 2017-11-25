--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.Monoid ((<>), mconcat)
import           Data.List ()
import qualified Data.Map ()
import           Data.List (isPrefixOf)
import           Data.Text (pack, unpack, replace, empty)
import           Hakyll
import           Hakyll.Web.Tags ()
import qualified System.FilePath.Posix as F

data BlogConfig = BlogConfig { root :: String
                             , protocol :: String
                             , title :: String
                             , author :: String
                             , bio :: String
                             , email :: String
                             , description :: String
                             , profilePic :: String
                             }

data SocialLink = SocialLink { name :: String
                             , url :: String
                             , icon :: String
                             }

--------------------------------------------------------------------------------

config :: BlogConfig
config = BlogConfig
  { root = "sakshamsharma.com"
  , protocol = "https"
  , title = "AceHack"
  , bio = "Programmer."
  , author = "Saksham Sharma"
  , email = "saksham0808@gmail.com"
  , description = "Reveries of a programmer"
  , profilePic = "https://avatars3.githubusercontent.com/u/10418596?v=3&s=460"
  }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = title config
    , feedDescription = description config
    , feedAuthorName  = author config
    , feedAuthorEmail = email config
    , feedRoot        = (protocol config) ++ "://" ++ (root config)
    }

links :: [SocialLink]
links = [ SocialLink { name = "GitHub", url = "https://github.com/sakshamsharma", icon = "fa-github" }
        , SocialLink { name = "LinkedIn", url = "https://in.linkedin.com/in/saksham-sharma", icon = "fa-linkedin" }
        , SocialLink { name = "Resume", url = "/cv", icon = "fa-file-pdf-o" }
        , SocialLink { name = "Rss", url = "/rss.xml", icon = "fa-rss" }
        ]

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

       match (fromList ["404.md", "CNAME", "favicon.ico"]) $ do
             route idRoute
             compile copyFileCompiler

       match "projects/**" $ do
         route projectRoute
         compile copyFileCompiler

       match "resume.pdf" $ do
         route idRoute
         compile copyFileCompiler

       match "templates/**" $ compile templateBodyCompiler

       let posts = recentFirst =<< loadAll (postPattern .&&. hasVersion "main")
       let staticPosts = recentFirst =<< loadAll (postPattern .&&. hasVersion "static")
       let recentPosts = fmap (take 5) staticPosts

       cats <- buildCategoriesNew postPattern (fromCapture "categories/*.html")
       tags <- buildTags postPattern (fromCapture "tags/*.html")

       let postCtx = dateField "date" "%B %e, %Y" <>
             constField "baseURL" ((protocol config) ++ "://" ++ (root config)) <> -- Need this here so we can access it inside for(posts)
             dateField "dateMap" "%Y-%m-%d" <>
             tagsField "tags" tags <>
             tagsField "cats" cats <>
             teaserField "teaser" "content" <>
             defaultContext

       let ctxWithInfo = fmap $ \rawposts ->
             constField "blogTitle" (title config) <>
             constField "bio" (bio config) <>
             constField "baseURL" ((protocol config) ++ "://" ++ (root config)) <>
             constField "author" (author config) <>
             constField "profilePic" (profilePic config) <>
             listField "recentPosts" postCtx recentPosts <>
             listField "links" linkCtx (sequence (map (\link -> makeItem(link)) links)) <>
             constField "postCount" (show $ length rawposts) <>
             defaultContext

       create ["index.html"] $ do
         route idRoute
         compile $ do
           simplePageCtx <- ctxWithInfo staticPosts
           let pageCtx = simplePageCtx <>
                         listField "posts" postCtx posts
           makeItem ""
             >>= loadAndApplyTemplate "templates/index.html"        pageCtx
             >>= loadAndApplyTemplate "templates/with-main.html"    pageCtx
             >>= loadAndApplyTemplate "templates/with-sidebar.html" pageCtx
             >>= loadAndApplyTemplate "templates/with-profile.html" pageCtx
             >>= loadAndApplyTemplate "templates/default.html"      pageCtx
             >>= relativizeUrls
             >>= cleanIndexHtmls

       match postPattern $ version "main" $ do
             route $ postRoute
             compile $ do
               simplePageCtx <- ctxWithInfo staticPosts
               let pageCtx = (teaserField "teaser" "content") <> simplePageCtx
               pandocCompiler
                     >>= saveSnapshot "content"
                     >>= loadAndApplyTemplate "templates/post.html"           pageCtx
                     >>= loadAndApplyTemplate "templates/with-wide-main.html" pageCtx
                     >>= loadAndApplyTemplate "templates/with-sidebar.html"   pageCtx
                     >>= loadAndApplyTemplate "templates/default.html"        pageCtx
                     >>= relativizeUrls

       -- For recentPosts inside posts
       match postPattern $ version "static" $ do
             compile $ do pandocCompiler

       tagsRules tags $ \tag pattern -> do
         let title = "Posts tagged \"" ++ tag ++ "\""
         route idRoute
         compile $ do
           let lessPosts = recentFirst =<< loadAll (pattern .&&. hasVersion "main")
           simplePageCtx <- ctxWithInfo staticPosts
           let pageCtx = simplePageCtx <>
                         constField "title" title <>
                         listField "posts" postCtx lessPosts
           makeItem ""
               >>= loadAndApplyTemplate "templates/index.html"        pageCtx
               >>= loadAndApplyTemplate "templates/with-main.html"    pageCtx
               >>= loadAndApplyTemplate "templates/with-sidebar.html" pageCtx
               >>= loadAndApplyTemplate "templates/with-profile.html" pageCtx
               >>= loadAndApplyTemplate "templates/default.html"      pageCtx
               >>= relativizeUrls

       match (fromList ["about.md"])$ do
         route $ cleanRoute True
         compile $ do
           simplePageCtx <- ctxWithInfo staticPosts
           let pageCtx = simplePageCtx
           pandocCompiler
             >>= loadAndApplyTemplate "templates/about.html"        pageCtx
             >>= loadAndApplyTemplate "templates/with-main.html"    pageCtx
             >>= loadAndApplyTemplate "templates/with-sidebar.html" pageCtx
             >>= loadAndApplyTemplate "templates/with-profile.html" pageCtx
             >>= loadAndApplyTemplate "templates/default.html"      pageCtx
             >>= relativizeUrls

       create ["sitemap.xml"] $ do
              route   idRoute
              compile $ do
                simplePageCtx <- ctxWithInfo staticPosts
                let pageCtx = simplePageCtx <>
                              listField "posts" postCtx posts
                makeItem ""
                 >>= loadAndApplyTemplate "templates/sitemap.xml" pageCtx
                 >>= cleanIndexHtmls

       create ["rss.xml"] $ do
           route idRoute
           compile $ do
               simplePageCtx <- ctxWithInfo staticPosts
               let feedCtx = postCtx <> simplePageCtx <> bodyField "description"
               pPosts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots ("posts/**" .&&. hasVersion "main") "content"
               renderRss myFeedConfiguration feedCtx pPosts

--------------------------------------------------------------------------------
linkCtx = field "linkname" (return . (\x -> name x) . itemBody) <>
          field "linkurl" (return . (\x -> url x) . itemBody) <>
          field "linkicon" (return . (\x -> icon x) . itemBody)

postPattern :: Pattern
postPattern = "posts/**"

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"

replacement :: String -> String
replacement = const "/"

modernPostPath :: String -> String
modernPostPath path =
  year ++ "/" ++ month ++ "/" ++ rest
  where
    fileName = F.takeBaseName path
    year = takeWhile (/= '-') $ fileName
    month = takeWhile (/= '-') . drop 1 . dropWhile (/= '-') $ fileName
    rest = dropWhile (\x -> isDigit x || x == '-') $ fileName

postRoute :: Routes
postRoute = (customRoute $ (\path -> modernPostPath $ toFilePath path))
  `composeRoutes` cleanRoute False

cleanRoute :: Bool -> Routes
cleanRoute isTopLevel =
  customRoute $
  (++ "/index.html") . takeWhile (/= '.') . (adjustPath isTopLevel) . toFilePath
  where
    adjustPath False = id
    adjustPath True = reverse . takeWhile (/= '/') . reverse

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath

-- | Add support for adding category directly to the metadata
-- | Helps avoid changing paths of posts while migrating from old blog
getCustomCat :: MonadMetadata m => Identifier -> m [String]
getCustomCat identifier = do
    metadata <- getMetadataField identifier "category"
    return $ maybe [] (map trim . splitAll ",") metadata

buildCategoriesNew :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildCategoriesNew = buildTagsWith getCustomCat

projectRoute :: Routes
projectRoute =
  idRoute `composeRoutes`
  (customRoute $ (++ "/index") . takeWhile (/= '.') . drop 9 . toFilePath ) `composeRoutes`
  setExtension "html"
