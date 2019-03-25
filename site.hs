--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List (intersperse)
import qualified Data.Map              ()
import           Data.Monoid           ((<>))
import qualified GHC.IO.Encoding       as E
import           Hakyll
import           Hakyll.Web.Tags       ()
import qualified System.FilePath.Posix as F
import           System.IO.Unsafe
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc

data BlogConfig = BlogConfig { root        :: String
                             , protocol    :: String
                             , title       :: String
                             , author      :: String
                             , bio         :: String
                             , email       :: String
                             , description :: String
                             , profilePic  :: String
                             }

data SocialLink = SocialLink { name :: String
                             , url  :: String
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
    , feedRoot        = protocol config ++ "://" ++ root config
    }

links :: [SocialLink]
links = [ SocialLink { name = "GitHub", url = "https://github.com/sakshamsharma", icon = "fa-github" }
        , SocialLink { name = "LinkedIn", url = "https://in.linkedin.com/in/saksham-sharma", icon = "fa-linkedin" }
        , SocialLink { name = "Resume", url = "/cv", icon = "fa-file-pdf-o" }
        , SocialLink { name = "RSS", url = "/rss.xml", icon = "fa-rss" }
        ]

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do

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

       let posts = recentFirst =<< loadAll (postPattern .&&. hasNoVersion)
       let staticPosts = recentFirst =<< loadAll (postPattern .&&. hasVersion "static")
       let recentPosts = fmap (take 5) staticPosts

       tags <- buildTags postPattern (fromCapture "tags/*.html")
       cats <- buildCategoriesNew postPattern (fromCapture "categories/*.html")

       let postCtx = dateField "date" "%B %e, %Y" <>
             constField "baseURL" (protocol config ++ "://" ++ root config) <> -- Need this here so we can access it inside for(posts)
             dateField "dateMap" "%Y-%m-%d" <>
             tagsField "tags" tags <>
             categoryFieldNew "category" cats <>
             teaserField "teaser" "content" <>
             defaultContext

       let ctxWithInfo = fmap $ \rawposts ->
             constField "blogTitle" (title config) <>
             constField "bio" (bio config) <>
             constField "baseURL" (protocol config ++ "://" ++ root config) <>
             constField "author" (author config) <>
             constField "profilePic" (profilePic config) <>
             listField "allCats" postCtx (return (collectTags cats)) <>
             listField "allTags" postCtx (return (collectTags tags)) <>
             listField "recentPosts" postCtx recentPosts <>
             listField "links" linkCtx (mapM makeItem links) <>
             constField "postCount" (show $ length rawposts) <>
             defaultContext

       create ["index.html"] $ do
         route idRoute
         compile $ do
           simplePageCtx <- ctxWithInfo staticPosts
           let pageCtx = simplePageCtx <>
                         constField "title" "Home" <>
                         constField "summary" "Saksham Sharma's Blog" <>
                         categoryFieldNew "category" cats <>
                         constField "showProfile" "" <>
                         listField "posts" postCtx posts
           makeItem ""
             >>= loadAndApplyTemplate "templates/index.html"        pageCtx
             >>= loadAndApplyTemplate "templates/default.html"      pageCtx
             >>= relativizeUrls
             >>= cleanIndexHtmls

       match postPattern $ do
             route postRoute
             compile $ do
               simplePageCtx <- ctxWithInfo staticPosts
               let pageCtx = teaserField "teaser" "content" <> postCtx <> simplePageCtx
               pandocCompilerWith defaultHakyllReaderOptions withToc
                     >>= saveSnapshot "content"
                     >>= loadAndApplyTemplate "templates/post.html"           pageCtx
                     >>= loadAndApplyTemplate "templates/default.html"        pageCtx
                     >>= relativizeUrls

       -- For recentPosts inside posts
       match postPattern $ version "static" $ do
             route postRoute
             compile pandocCompiler

       let tagPageGen pagetitle pat = do
             lessPosts <- recentFirst =<< loadAll pat
             simplePageCtx <- ctxWithInfo staticPosts
             let pageCtx = simplePageCtx <>
                           constField "showProfile" "lala" <>
                           constField "title" pagetitle <>
                           constField "summary" ("Filtered items, " ++ pagetitle) <>
                           constField "pagetitle" pagetitle <>
                           listField "posts" (postCtx <> constField "pagetitle" pagetitle) (return lessPosts)
             makeItem ""
               >>= loadAndApplyTemplate "templates/archive.html"      pageCtx
               >>= loadAndApplyTemplate "templates/default.html"      pageCtx
               >>= relativizeUrls

       tagsRules cats $ \tag pat -> do
         let pagetitle = "Posts in category \"" ++ tag ++ "\""
         route idRoute
         compile $ tagPageGen pagetitle pat

       tagsRules tags $ \tag pat -> do
         let pagetitle = "Posts tagged \"" ++ tag ++ "\""
         route idRoute
         compile $ tagPageGen pagetitle pat

       match (fromList ["about.md"])$ do
         route $ cleanRoute True
         compile $ do
           simplePageCtx <- ctxWithInfo staticPosts
           let pageCtx = simplePageCtx <>
                         constField "showProfile" ""
           pandocCompiler
             >>= loadAndApplyTemplate "templates/about.html"        pageCtx
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
                   loadAllSnapshots (postPattern .&&. hasNoVersion) "content"
               renderRss myFeedConfiguration feedCtx pPosts

--------------------------------------------------------------------------------
linkCtx = field "linkname" (return . name . itemBody) <>
          field "linkurl" (return . url . itemBody) <>
          field "linkicon" (return . icon . itemBody)

collectTags tags = map (\(t, _) -> Item (tagsMakeId tags t) t) (tagsMap tags)

postPattern :: Pattern
postPattern = "posts/*.md" .||. "posts/*.org"

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
    where
      pat = "/index.html"

replacement :: String -> String
replacement = const "/"

modernPostPath :: String -> String
modernPostPath path =
  year ++ "/" ++ month ++ "/" ++ rest
  where
    fileName = F.takeBaseName path
    year = takeWhile (/= '-') fileName
    month = takeWhile (/= '-') . drop 1 . dropWhile (/= '-') $ fileName
    rest = dropWhile (\x -> isDigit x || x == '-') fileName

postRoute :: Routes
postRoute = customRoute (modernPostPath . toFilePath)
  `composeRoutes` cleanRoute False

cleanRoute :: Bool -> Routes
cleanRoute isTopLevel =
  customRoute $
  (++ "/index.html") . takeWhile (/= '.') . adjustPath isTopLevel . toFilePath
  where
    adjustPath False = id
    adjustPath True  = reverse . takeWhile (/= '/') . reverse

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

simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ H.a H.! A.href (H.toValue $ toUrl filePath) $ H.toHtml tag

categoryFieldNew = tagsFieldWith getCustomCat simpleRenderLink (mconcat . intersperse ", ")

projectRoute :: Routes
projectRoute =
  idRoute `composeRoutes`
  customRoute ((++ "/index") . takeWhile (/= '.') . drop 9 . toFilePath) `composeRoutes`
  setExtension "html"

-- | Do not judge me :) I needed these one time.
debugIO msg result = putStrLn msg >> return result
debug s r = unsafePerformIO $ debugIO s r

withToc = defaultHakyllWriterOptions
          { writerTableOfContents = True
          , writerTemplate        = Just "<div class=\"toc\">$toc$</div>\n$body$"
          , writerTOCDepth        = 2
          }
