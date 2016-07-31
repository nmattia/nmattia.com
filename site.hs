--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Control.Monad (forM_)
import Hakyll
import Hakyll.Web.Pandoc (pandocCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/default.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/font-awesome/**/*" $ do
        route idRoute
        compile copyFileCompiler

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

    forM_ [ "cv.pdf"
          , "parallel-dna.pdf"
          , "regular-strands.pdf"
          , "schroedinger.pdf"
          , "stone-age.pdf" ]
          (\f -> match f (route idRoute >> compile copyFileCompiler))


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
