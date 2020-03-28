--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           System.FilePath
import           Control.Monad
import           Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)
import           Data.Either (either)
import           Data.Binary (Binary)
import           Data.Typeable
import           Data.Maybe (fromJust, isJust)
import qualified Data.Map               as M
import qualified Data.Char              as Char
import           Hakyll

import qualified Text.CSL               as CSL
import           Text.CSL.Pandoc (processCites)
import           Text.Pandoc
import           Text.Pandoc.PDF (makePDF)      
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

cslTemplate :: String
cslTemplate = "default"

--------------------------------------------------------------------------------
writerOptions :: Maybe String -> WriterOptions
writerOptions Nothing = defaultHakyllWriterOptions 
writerOptions _  = defaultHakyllWriterOptions {
    writerExtensions = (writerExtensions defaultHakyllWriterOptions) <> extensionsFromList [
        Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, -- math
        Ext_backtick_code_blocks, Ext_fenced_code_attributes, -- code
        Ext_auto_identifiers, -- references
        Ext_yaml_metadata_block -- additional support for metadata
    ],
    writerHTMLMathMethod = MathJax ""
}

addMeta :: String -> MetaValue -> Pandoc -> Pandoc
addMeta k v (Pandoc (Meta m) a) = Pandoc (Meta $ M.insert k v m) a

addLinkCitations :: Pandoc -> Pandoc
addLinkCitations = addMeta "link-citations"          (MetaBool True)
                 . addMeta "reference-section-title" (MetaString "References")

capitalizeFirst :: String -> String
capitalizeFirst s = Char.toUpper (head s) : tail s

--------------------------------------------------------------------------------
-- Although I've edited the code quite a bit,
-- all thanks go to vjeranc (https://github.com/jaspervdj/hakyll/issues/471)
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

--------------------------------------------------------------------------------
-- Inspired by renderTagList
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#renderTagList)
categoriesField :: String  
                -> Tags
                -> Context a
categoriesField k tags = field k $ \_ -> renderTags makeLink (intercalate " ") tags
    where makeLink tag _ _ _ _ = renderHtml (fromJust $ simpleRenderCatLink tag Nothing)

-- Inspired by categoryField
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#categoryField)
categoryField' :: String     -- ^ Destination key
               -> Tags       -- ^ Tags
               -> Context a  -- ^ Context
categoryField' = tagsFieldWith getCategory simpleRenderCatLink (mconcat . intersperse ", ")
    where getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- Inspired by simpleRenderLink
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#simpleRenderLink)   
simpleRenderCatLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderCatLink tag _ = Just $ H.a ! A.href (toValue $ '/':tag) $ toHtml (capitalizeFirst tag)

--------------------------------------------------------------------------------  
-- Inspired by relativizeUrls, relativizeUrlsWith
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Html.RelativizeUrls.html#relativizeUrlsWith)
removeHTMLExtensions :: Item String -> Compiler (Item String)
removeHTMLExtensions item = do
    currentRoute <- getRoute $ itemIdentifier item
    return $ case currentRoute of
        Nothing -> item
        Just _  -> fmap (withUrls remExt) item
          -- is relative?
    where hasExt x = "." `isPrefixOf` x && ".html" `isSuffixOf` x
          remExt x = if hasExt x then dropExtension x else x

--------------------------------------------------------------------------------
pandocCompile :: Maybe String -- use MathJax?
              -> Maybe String -- use Citations?
              -> Maybe String -- number sections?
              -> Compiler (Item String)
pandocCompile mathJax biblioFile nsecs = 
    maybe (
        pandocCompilerWith defaultHakyllReaderOptions 
        ((writerOptions mathJax) { writerNumberSections = isJust nsecs })
    ) 
    (\biblioFileName ->
        pandocBiblioCompilerWithTransform defaultHakyllReaderOptions 
            ((writerOptions mathJax) { writerNumberSections = isJust nsecs })
            ("csl" </> cslTemplate <.> "csl")
            ("bib" </> biblioFileName <.> "bib")
            addLinkCitations
    ) biblioFile

loadMaybe :: (Binary a, Typeable a) 
          => Maybe FilePath
          -> Compiler (Maybe (Item a))
loadMaybe Nothing = pure Nothing
loadMaybe (Just fp) = (load $ fromFilePath fp) >>= pure . Just

pandocRead :: Item CSL
           -> Maybe (Item Biblio)  -- use Citations ?
           -> (Item String -> Compiler (Item Pandoc))
pandocRead _ Nothing = readPandoc
pandocRead csl (Just bib) = 
    readPandocBiblioWithTransform 
        defaultHakyllReaderOptions
        csl
        bib
        addLinkCitations

main :: IO ()
main = do
    temp <- readFile "template.tex"
    hakyll $ do
        match (
            "images/*" .||. fromList [".htaccess", "browserconfig.xml", "robots.txt", "favicon.ico"]
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
        tags <- buildTags "posts/**" (fromCapture "tags/*.html")
        let pageCtx = categoriesField "cats" cats <> defaultContext
            postCtx =  categoryField' "category" cats
                    <> tagsField "tags" tags
                    <> dateField "date" "%B %e, %Y"
                    <> modificationTimeField "modificationDate" "%B %e, %Y" 
                    <> defaultContext

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

                let pageCtx' = if matchExt == ".tex" && isJust matchUrl
                               then constField "pdf" (
                                   dropExtension (fromJust matchUrl) ++ ".pdf"
                               ) <> pageCtx
                               else pageCtx
                            in
                    (pandocCompile mathJax biblioFile nsecs
                    >>= loadAndApplyTemplate "templates/post.html"    postCtx
                    >>= loadAndApplyTemplate "templates/default.html" pageCtx'
                    >>= relativizeUrls >>= removeHTMLExtensions)

        -- TeX pdf compilation
        match "posts/**.tex" $ version "pdf" $ do
            route   $ gsubRoute "posts/" (const "") `composeRoutes` setExtension ".pdf" 
            compile $ do   
                matchId <- getUnderlying
                body    <- getResourceBody
                biblio  <- getMetadataField matchId "bibliography"
                nsecs   <- getMetadataField matchId "numbersections"

                csl     <- load $ fromFilePath $ "csl" </> cslTemplate <.> "csl"
                bib     <- loadMaybe $ (biblio >>= (\fn -> Just $ "bib" </> fn <.> "bib"))
                pan     <- (pandocRead csl bib) body

                withItemBody
                    (\p -> unsafeCompiler $ runIOorExplode $ do 
                        either id id <$> makePDF "pdflatex" [] writeLaTeX
                            defaultHakyllWriterOptions { 
                                writerTemplate = Just temp,
                                writerNumberSections = isJust nsecs
                            }
                            p
                    ) pan

        -- Category indices compilation
        tagsRules cats $ \tag pattern -> do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
                let categoryName = capitalizeFirst tag
                    archiveCtx   = listField "posts" postCtx (return posts)
                                <> constField "title" categoryName
                                <> pageCtx

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- Generate static pages
        match "pages/**" $ do
            route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
            compile $ do
                getResourceBody
                    >>= applyAsTemplate pageCtx
                    >>= loadAndApplyTemplate "templates/default.html" pageCtx
                    >>= relativizeUrls >>= removeHTMLExtensions

        -- Generate homepage
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll ("posts/**" .&&. hasNoVersion)
                let indexCtx = listField "posts" postCtx (return $ take 5 posts) 
                            <> pageCtx

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls >>= removeHTMLExtensions
