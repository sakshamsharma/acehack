--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List (intercalate)
import           Hakyll
import           Hakyll.Web.Tags


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

       match "assets/js/*" $ do
             route assetsRoute
             compile copyFileCompiler
     
       match "assets/css/*" $ do
             route assetsRoute
             compile compressCssCompiler

       match "assets/images/*" $ do
             route assetsRoute
             compile copyFileCompiler

       match (fromList ["about.md", "contact.md"]) $ do
             route   $ setExtension "html"
             compile $ pandocCompiler
                     >>= loadAndApplyTemplate "templates/content.html" postCtx
                     >>= loadAndApplyTemplate "templates/default.html" defaultContext
                     >>= relativizeUrls

       tags <- buildTags "posts/*" (fromCapture "tags/*.html")
       match "posts/*" $ do
             route $ setExtension "html"
             compile $ do
               let postCtxTagged =
                     tagsField "tagsCtx" tags `mappend`
                     postCtx
               pandocCompiler
                     >>= loadAndApplyTemplate "templates/post.html"    postCtxTagged
                     >>= loadAndApplyTemplate "templates/default.html" postCtxTagged
                     >>= relativizeUrls

       create ["archive.html"] $ do
              route idRoute
              compile $ do
                      posts <- recentFirst =<< loadAll "posts/*"
                      let archiveCtx =
                            tagsField "tags" tags `mappend`
                            listField "posts" postCtx (return posts) `mappend`
                            constField "title" "Archives"            `mappend`
                            field "tagList" (\_ -> renderTagCloud 70 140 tags) `mappend`
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
                 `mappend` tagsField "tags" tags
                 `mappend` listField "posts" postCtx (return posts)
                 `mappend` defaultContext

           makeItem ""
             >>= loadAndApplyTemplate "templates/post-list.html" ctx
             >>= loadAndApplyTemplate "templates/default.html" ctx
             >>= relativizeUrls


       match "index.html" $ do
             route idRoute
             compile $ do
                     posts <- recentFirst =<< loadAll "posts/*"
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


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend`
          defaultContext

assetsRoute :: Routes
assetsRoute = customRoute $ (\x -> x :: String) . drop 7 . toFilePath
