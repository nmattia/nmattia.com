--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Control.Monad (forM_)
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

    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
          let posts = recentFirst =<< loadAllSnapshots "posts/*" "content"
          let indexCtx = listField "posts" postCtx posts <> defaultContext
          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match ("posts/*.md" .||. "posts/*.lhs") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/post.html" defaultContext
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

    let feedCtx = postCtx `mappend` bodyField "description"

    let posts =
          fmap (take 10) .
          recentFirst =<< loadAllSnapshots "posts/*" "content"

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
