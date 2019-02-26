--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           System.FilePath
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
        Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, -- math
        Ext_backtick_code_blocks, Ext_fenced_code_attributes -- code
    ],
    writerHTMLMathMethod = MathJax ""
}

--------------------------------------------------------------------------------
-- Although I've edited the code quite a bit,
-- all thanks go to vjeranc (https://github.com/jaspervdj/hakyll/issues/471)

addLinkCitations :: Pandoc -> Pandoc
addLinkCitations = addMeta "link-citations"          (MetaBool True)
                 . addMeta "reference-section-title" (MetaString "References")

addMeta :: String -> MetaValue -> Pandoc -> Pandoc
addMeta k v (Pandoc (Meta m) a) = Pandoc (Meta $ M.insert k v m) a

readPandocBiblioWithTransform :: ReaderOptions
                              -> Item CSL
                              -> Item Biblio
                              -> (Pandoc -> Pandoc)
                              -> Item String
                              -> Compiler (Item Pandoc)
readPandocBiblioWithTransform ropt csl biblio f item = do
    style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = processCites style refs (f pandoc)

    return $ fmap (const pandoc') item

pandocBiblioCompilerWithTransform :: ReaderOptions -> WriterOptions
                                  -> String -- CSL FilePath
                                  -> String -- BIB FilePath
                                  -> (Pandoc -> Pandoc)
                                  -> Compiler (Item String)
pandocBiblioCompilerWithTransform ropt wopt cslFileName bibFileName f = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM (writePandocWith wopt)
        (getResourceBody >>= readPandocBiblioWithTransform ropt csl bib f)

pandocCompile :: Maybe String -- use Math Jax?
              -> Maybe String -- use Citations?
              -> Compiler (Item String)
pandocCompile mathJax biblioFile = 
    maybe (pandocCompilerWith defaultHakyllReaderOptions (writerOptions mathJax)) (\biblioFileName ->
    pandocBiblioCompilerWithTransform defaultHakyllReaderOptions (writerOptions mathJax)
        "csl/journal-of-mathematical-physics.csl"
        ("bib" </> biblioFileName <.> "bib")
        addLinkCitations
    ) biblioFile

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
            
            pandocCompile mathJax biblioFile
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
