module Helpers (
  capitalizeFirst,
  removeHTMLExtensions,
  loadMaybe
) where

import Hakyll (Item, Compiler, itemIdentifier, load, withUrls, getRoute, fromFilePath)
import System.FilePath (dropExtension)

import           Data.Typeable
import           Data.Binary (Binary)
import qualified Data.Char              as Char
import           Data.List (isPrefixOf, isSuffixOf)

capitalizeFirst :: String -> String
capitalizeFirst s = Char.toUpper (head s) : tail s

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

loadMaybe :: (Binary a, Typeable a)
  => Maybe FilePath
  -> Compiler (Maybe (Item a))
loadMaybe Nothing = pure Nothing
loadMaybe (Just fp) = (load $ fromFilePath fp) >>= pure . Just
