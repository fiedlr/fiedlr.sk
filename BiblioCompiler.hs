module BiblioCompiler (
  pandocRead,
  pandocCompile,
  cslTemplate
) where

import Hakyll
import System.FilePath
import Data.Maybe (isJust, fromJust)
import qualified Data.Map               as M
import           Text.Pandoc
import           Control.Monad
import qualified Text.CSL               as CSL
import           Text.CSL.Pandoc (processCites)

cslTemplate :: String
cslTemplate = "default"

addMeta :: String -> MetaValue -> Pandoc -> Pandoc
addMeta k v (Pandoc (Meta m) a) = Pandoc (Meta $ M.insert k v m) a

addLinkCitations :: Pandoc -> Pandoc
addLinkCitations = addMeta "link-citations"          (MetaBool True)
                 . addMeta "reference-section-title" (MetaString "References")

writerOptions :: Maybe String -> WriterOptions
writerOptions Nothing = defaultHakyllWriterOptions
writerOptions _  = defaultHakyllWriterOptions {
    writerExtensions = writerExtensions defaultHakyllWriterOptions <> extensionsFromList [
        Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, -- math
        Ext_backtick_code_blocks, Ext_fenced_code_attributes, -- code
        Ext_auto_identifiers, -- references
        Ext_yaml_metadata_block -- additional support for metadata
    ],
    writerHTMLMathMethod = MathJax ""
}

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
