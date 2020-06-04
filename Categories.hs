{-# LANGUAGE OverloadedStrings          #-}
module Categories (
  categoriesField,
  categoryField
) where

import Hakyll (Tags (..), Context (..), toFilePath, tagsFieldWith, renderTags, field)
import Helpers (capitalizeFirst)

import qualified Data.Char              as Char
import           Data.Maybe (fromJust)
import           Data.List (intercalate, intersperse, isPrefixOf, isSuffixOf)

import           System.FilePath (takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

-- Inspired by renderTagList
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#renderTagList)
categoriesField :: String
                -> Tags
                -> Context a
categoriesField k tags = field k $ \_ -> renderTags makeLink (intercalate " ") tags
    where makeLink tag _ _ _ _ = renderHtml (fromJust $ simpleRenderCatLink tag Nothing)

-- Inspired by categoryField
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#categoryField)
categoryField :: String     -- ^ Destination key
               -> Tags       -- ^ Tags
               -> Context a  -- ^ Context
categoryField = tagsFieldWith getCategory simpleRenderCatLink (mconcat . intersperse ", ")
    where getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- Inspired by simpleRenderLink
-- (https://jaspervdj.be/hakyll/reference/src/Hakyll.Web.Tags.html#simpleRenderLink)
simpleRenderCatLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderCatLink tag _ = Just $ H.a ! A.href (toValue $ '/':tag) $ toHtml (capitalizeFirst tag)
