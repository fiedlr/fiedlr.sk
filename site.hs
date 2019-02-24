--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           System.FilePath
import           Data.Maybe
import qualified Data.Map               as M
import           Data.Monoid (mappend)
import           Hakyll
import qualified Text.CSL               as CSL
import           Text.CSL.Pandoc (processCites)
import           Text.Pandoc
import           Control.Monad

--------------------------------------------------------------------------------
writerOptions :: Maybe String -> WriterOptions
writerOptions Nothing = defaultHakyllWriterOptions 
writerOptions _       = defaultHakyllWriterOptions {
    writerExtensions = (writerExtensions defaultHakyllWriterOptions) <> extensionsFromList [
        Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros
    ],
    writerHTMLMathMethod = MathJax ""
}

--------------------------------------------------------------------------------
-- Courtesy of https://github.com/jaspervdj/hakyll/issues/471
addLinkCitations (Pandoc meta a) =
    let prevMap = unMeta meta
        newMap = M.insert "link-citations" (MetaBool True) 
               $ M.insert "reference-section-title" (MetaString "References") prevMap
        newMeta = Meta newMap
    in  Pandoc newMeta a

readPandocBiblioWithLinks :: ReaderOptions
                          -> Item CSL
                          -> Item Biblio
                          -> Item String
                          -> Compiler (Item Pandoc)
readPandocBiblioWithLinks ropt csl biblio item = do
    style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = processCites style refs (addLinkCitations pandoc)

    return $ fmap (const pandoc') item

pandocBiblioCompilerWith :: ReaderOptions 
                         -> WriterOptions
                         -> String 
                         -> String 
                         -> Compiler (Item String)
pandocBiblioCompilerWith ropt wopt cslFileName bibFileName = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM (writePandocWith wopt)
        (getResourceBody >>= readPandocBiblioWithLinks ropt csl bib)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match (fromList [".htaccess", "browserconfig.xml", "robots.txt", "favicon.ico"])  $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "csl/*" $ compile cslCompiler
    match "bib/*" $ compile biblioCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
            id         <- getUnderlying
            mathJax    <- getMetadataField id "mathjax"
            biblioFile <- getMetadataField id "bibliography"
            
            maybe (pandocCompilerWith defaultHakyllReaderOptions (writerOptions mathJax)) (\biblioFileName ->
                pandocBiblioCompilerWith defaultHakyllReaderOptions (writerOptions mathJax)
                    "csl/journal-of-mathematical-physics.csl"
                    ("bib" </> biblioFileName <.> "bib")
                ) biblioFile
            
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["essays.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Essays"              <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext
