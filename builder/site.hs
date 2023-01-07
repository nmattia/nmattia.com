--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isNothing)
import Data.Monoid (mappend)
import Control.Monad (forM_, filterM)
import Hakyll
import Hakyll.Web.Pandoc (pandocCompiler)
import System.FilePath  (joinPath, splitPath)

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "nmattia's blog"
    , feedDescription = "Experiments with software"
    , feedAuthorName  = "Nicolas Mattia"
    , feedAuthorEmail = "nicolas@nmattia.com"
    , feedRoot        = "http://www.nmattia.com"
    }

dropPreview :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
dropPreview = filterM isReleased
    where
    isReleased item = do
        let identifier = itemIdentifier item
        preview <- getMetadataField identifier "preview"
        pure $ isNothing preview

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "styles/default.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "styles/fonts/**/*" $ do
        route idRoute
        compile copyFileCompiler

    match "blog.html" $ do
      route idRoute
      compile copyFileCompiler

    let posts = loadAllSnapshots "posts/*" "content"
                  >>= recentFirst
                  >>= dropPreview

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
          let indexCtx = listField "posts" postCtx posts <> defaultContext
          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/default.html" noTitleContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match ("posts/*.md" .||. "posts/*.lhs") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/post.html" postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

    let feedCtx = postCtx `mappend` bodyField "description"

    let feedPosts = fmap (take 10) posts

    create ["atom.xml"] $ do
      route idRoute
      compile $ renderAtom myFeedConfiguration feedCtx =<< posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ renderAtom myFeedConfiguration feedCtx =<< posts

    match "material/index.html" $ do
        route idRoute
        compile copyFileCompiler

    match "material/**/*" $ do
        route idRoute
        compile copyFileCompiler

    match "icons/*" $ do
        route dropIconsRoute
        compile copyFileCompiler

    forM_ [ "resume.pdf"
          , "resume.html"
          , "parallel-dna.pdf"
          , "regular-strands.pdf"
          , "schroedinger.pdf"
          , "stone-age.pdf"
          , "boot"
          ]
          (\f -> match f (route idRoute >> compile copyFileCompiler))

--------------------------------------------------------------------------------
-- | Drop the `icons/` part from a route.
dropIconsRoute :: Routes
dropIconsRoute = customRoute $ \ident ->
    let path0 = toFilePath ident in
    case splitPath path0 of
        "icons/" : path1 -> joinPath path1
        _                  -> path0

postCtx :: Context String
postCtx =
    teaserField "teaser" "content" <>
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- Simplest way to drop the "title"; used as a workaround for the index page's
-- title
noTitleContext :: Context String
noTitleContext =
    bodyField     "body"     `mappend`
    metadataField            `mappend`
    urlField      "url"      `mappend`
    pathField     "path"     `mappend`
    missingField
