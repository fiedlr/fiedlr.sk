module PdfCompiler (
  pdfCompile
) where

import Hakyll (Compiler (..), Item (..), defaultHakyllWriterOptions, unsafeCompiler)

import Data.Maybe (isJust)
import Data.ByteString.Lazy (ByteString (..))

import Text.Pandoc
import Text.Pandoc.PDF (makePDF)

pdfCompile :: String
           -> Maybe String
           -> Pandoc
           -> Compiler ByteString
pdfCompile temp nsecs pan = unsafeCompiler $ runIOorExplode $
  either id id <$> makePDF "pdflatex" [] writeLaTeX
      defaultHakyllWriterOptions {
          writerTemplate = Just temp,
          writerNumberSections = isJust nsecs
      }
      pan
