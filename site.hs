--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           System.FilePath
import           Data.Maybe (fromJust, isJust)

import Helpers (removeHTMLExtensions, capitalizeFirst, loadMaybe)
import PdfCompiler
import qualified Categories as Categories
import qualified BiblioCompiler as Biblio

defaultTeaser :: String
defaultTeaser = "coding a bit"

texTemplate :: String
texTemplate = "template"

customConf :: Configuration
customConf = defaultConfiguration { destinationDirectory = "docs" }

main :: IO ()
main = do
    temp <- readFile $ texTemplate <.> "tex"
    hakyllWith customConf $ do
        match (
            "images/*" .||. fromList [
                ".htaccess",
                "browserconfig.xml",
                "robots.txt",
                "favicon.ico",
                "CNAME",
                "googlef0dd479df4515f1e.html"
            ]
            ) $ do
                route   idRoute
                compile copyFileCompiler
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler
        match "csl/*" $ compile cslCompiler
        match "bib/*" $ compile biblioCompiler
        match "templates/*" $ compile templateBodyCompiler

        cats <- buildCategories "posts/**" (fromCapture "*/index.html")
        tags <- buildTags "posts/**" (fromCapture "topics/*.html")
        let pageCtx =  Categories.categoriesField "cats" cats
                    <> defaultContext
            postCtx =  Categories.categoryField "category" cats
                    <> dateField "date" "%B %e, %Y"
                    <> modificationTimeField "modificationDate" "%B %e, %Y"
                    <> defaultContext

        -- Category indices compilation
        tagsRules cats $ \cat pattern -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
                let archiveCtx   = listField "posts" postCtx (return posts)
                                <> constField "title" (capitalizeFirst cat)
                                <> constField "desc" defaultTeaser
                                <> pageCtx

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- Tag indices compilation
        tagsRules tags $ \tag pattern -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
                let archiveCtx   = listField "posts" postCtx (return posts)
                                <> constField "title" ('#' : tag)
                                <> constField "desc" defaultTeaser
                                <> pageCtx

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- Post compilation
        match "posts/**" $ do
            route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension ".html"
            compile $ do
                matchId    <- getUnderlying
                matchExt   <- getUnderlyingExtension
                matchUrl   <- getRoute matchId

                mathJax    <- getMetadataField matchId "mathjax"
                biblioFile <- getMetadataField matchId "bibliography"
                nsecs      <- getMetadataField matchId "numbersections"
                teaser     <- getMetadataField matchId "teaser"

                let excerpt  = maybe defaultTeaser id teaser
                    pdf      = if matchExt == ".tex" && isJust matchUrl
                               then constField "pdf" (
                                   dropExtension (fromJust matchUrl) <.> "pdf"
                               ) <> defaultContext
                               else defaultContext
                    postCtx' = tagsField "tags" tags <> pdf <> postCtx
                    pageCtx' = pdf <> pageCtx
                            <> constField "teaser" excerpt
                            <> constField "desc"   (stripTags excerpt)

                Biblio.pandocCompile mathJax biblioFile nsecs
                    >>= loadAndApplyTemplate "templates/post.html"    postCtx'
                    >>= loadAndApplyTemplate "templates/default.html" pageCtx'
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- TeX pdf compilation
        match "posts/**.tex" $ version "pdf" $ do
            route   $ gsubRoute "posts/" (const "") `composeRoutes` setExtension ".pdf"
            compile $ do
                matchId <- getUnderlying
                body    <- getResourceBody
                biblio  <- getMetadataField matchId "bibliography"
                nsecs   <- getMetadataField matchId "numbersections"

                csl     <- load $ fromFilePath $ "csl" </> Biblio.cslTemplate <.> "csl"
                bib     <- loadMaybe $ (biblio >>= (\fn -> Just $ "bib" </> fn <.> "bib"))
                pan     <- (Biblio.pandocRead csl bib) body

                withItemBody (pdfCompile temp nsecs) pan

        -- Generate static pages
        match "pages/**" $ do
            route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
            compile $ do
                let pageCtx' = constField "desc" defaultTeaser
                            <> tagCloudField "tagCloud" 20 80 tags
                            <> pageCtx

                getResourceBody
                    >>= applyAsTemplate pageCtx'
                    >>= loadAndApplyTemplate "templates/default.html" pageCtx'
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- Generate homepage
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ("posts/**" .&&. hasNoVersion)
                let indexCtx = listField "posts" postCtx (return $ take 5 posts)
                            <> constField "desc" defaultTeaser
                            <> pageCtx

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls >>= removeHTMLExtensions
