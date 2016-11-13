--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List (intercalate)
import qualified Data.Map as M
import           Hakyll
import           Hakyll.Web.Tags


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

       match (fromList ["404.md", "CNAME"]) $ do
             route idRoute
             compile copyFileCompiler

       match (fromList ["about.md", "contact.md"]) $ do
             route   $ setExtension "html"
             compile $ pandocCompiler
                     >>= loadAndApplyTemplate "templates/content.html" postCtx
                     >>= loadAndApplyTemplate "templates/default.html" defaultContext
                     >>= relativizeUrls

       tags <- buildTags "posts/**" (fromCapture "tags/*.html")
       categories <- buildCategoriesNew "posts/**" (fromCapture "categories/*.html")

       match "posts/**" $ do
             route $ setExtension "html"
             compile $ do
               let postCtxTagged =
                     tagsField "tagsCtx" tags `mappend`
                     postCtx
               pandocCompiler
                     >>= loadAndApplyTemplate "templates/post.html"    postCtxTagged
                     >>= loadAndApplyTemplate "templates/default.html" postCtxTagged
                     >>= relativizeUrls

       match "projects/**" $ do
         route projectRoute
         compile copyFileCompiler

       match "resume.pdf" $ do
         route idRoute
         compile copyFileCompiler

       create ["blog/index.html", "archive.html"] $ do
              route idRoute
              compile $ do
                      posts <- recentFirst =<< loadAll "posts/**"
                      let archiveCtx =
                            tagsField "tags" tags `mappend`
                            listField "posts" postCtx (return posts) `mappend`
                            constField "title" "Archives"            `mappend`
                            field "tagList" (\_ -> renderTagCloud 70 140 categories) `mappend`
                            defaultContext

                      makeItem ""
                        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls

       tagsRules tags $ \tag pattern -> do
         let title = "Posts tagged \"" ++ tag ++ "\""
         route idRoute
         compile $ do
           posts <- recentFirst =<< loadAll pattern
           let ctx = constField "title" title
                 `mappend` constField "extra" title
                 `mappend` field "tagList" (\_ -> renderTagCloud 70 140 categories)
                 `mappend` listField "posts" postCtx (return posts)
                 `mappend` defaultContext

           makeItem ""
             >>= loadAndApplyTemplate "templates/post-list.html" ctx
             >>= loadAndApplyTemplate "templates/default.html" ctx
             >>= relativizeUrls

       tagsRules categories $ \tag pattern -> do
           let title = "Posts in category \"" ++ tag ++ "\""
           route idRoute
           compile $ do
               posts <- recentFirst =<< loadAll pattern
               let ctx = constField "title" title
                     `mappend` constField "extra" title
                     `mappend` field "tagList" (\_ -> renderTagCloud 70 140 categories)
                     `mappend` listField "posts" postCtx (return posts)
                     `mappend` defaultContext

               makeItem ""
                   >>= loadAndApplyTemplate "templates/post-list.html" ctx
                   >>= loadAndApplyTemplate "templates/default.html" ctx
                   >>= relativizeUrls

       match "index.html" $ do
             route idRoute
             compile $ do
                     posts <- recentFirst =<< loadAll "posts/**"
                     let indexCtx =
                             listField "posts" postCtx (return posts) `mappend`
                             constField "title" "Home"                `mappend`
                             defaultContext

                     getResourceBody
                         >>= applyAsTemplate indexCtx
                         >>= loadAndApplyTemplate "templates/content.html" indexCtx
                         >>= loadAndApplyTemplate "templates/default.html" indexCtx
                         >>= relativizeUrls

       match "templates/*" $ compile templateBodyCompiler

       create ["sitemap.xml"] $ do
              route   idRoute
              compile $ do
                posts <- recentFirst =<< loadAll "posts/**"
                let sitemapCtx =
                        listField "posts" postCtx (return posts)   `mappend`
                        defaultContext
                makeItem ""
                 >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                 >>= cleanIndexHtmls

--------------------------------------------------------------------------------
cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
replacement = const "/"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend`
          defaultContext

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath

projectRoute :: Routes
projectRoute =
  idRoute `composeRoutes`
  (customRoute $ (++ "/index") . takeWhile (/= '.') . drop 9 . toFilePath ) `composeRoutes`
  setExtension "html"

-- | Add support for adding category directly to the metadata
-- | Helps avoid changing paths of posts while migrating from old blog
getCustomCat :: MonadMetadata m => Identifier -> m [String]
getCustomCat identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup "category" metadata

buildCategoriesNew :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildCategoriesNew = buildTagsWith getCustomCat
