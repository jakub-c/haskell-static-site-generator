{-# LANGUAGE OverloadedStrings #-}

module SiteGenerator.Core
    ( makeSlug
    , extractWikiLinks
    , replaceWikiLinks
    , makeHtmlLink
    ) where

import qualified Data.Text as T
import Data.Text (Text)

-- | Convert text to a URL-safe slug
-- Examples:
--   makeSlug "Hello World!" -> "hello-world"
--   makeSlug "Multiple   Spaces" -> "multiple-spaces"
--   makeSlug "Unicode ★ chars" -> "unicode-chars"
makeSlug :: Text -> Text
makeSlug text = 
  let slug = T.intercalate "-"                -- Join parts with hyphens
           . filter (not . T.null)            -- Remove empty parts
           . T.split (not . isAsciiAlphaNum)   -- Split on non-ASCII alphanumeric chars
           . T.toLower                        -- Convert to lowercase
           $ text
  in if T.null slug 
     then error $ "Cannot create slug from empty or invalid text: " ++ T.unpack text
     else slug
  where
    -- Only ASCII letters and digits are considered safe for URLs
    isAsciiAlphaNum c = (c >= 'a' && c <= 'z') || 
                       (c >= 'A' && c <= 'Z') || 
                       (c >= '0' && c <= '9')

-- | Extract all wiki links from text content
-- Wiki links are in the format [[Link Text]]
extractWikiLinks :: Text -> [Text]
extractWikiLinks text = reverse (go text [])  -- Reverse to maintain original order
    where
        go t acc
            | T.null t = acc
            | otherwise =
                case T.breakOn "[[" t of
                    (_, rest)
                        | T.null rest -> acc -- No more wiki links
                        | otherwise ->
                            let afterOpen = T.drop 2 rest
                                (linkText, afterLink) = T.breakOn "]]" afterOpen
                                remaining = T.drop 2 afterLink -- Drop the closing ]]
                            in go remaining (linkText : acc)

-- | Replace wiki links [[text]] with HTML links
replaceWikiLinks :: Text -> Text
replaceWikiLinks text = processText text
  where
    -- Break into parts and process each
    processText :: Text -> Text
    processText input =
        case T.splitOn "[[" input of
            [] -> ""                        -- Handle empty list case
            [singlePart] -> singlePart
            -- Found [[ - process parts
            firstPart:otherParts ->
                T.concat $ firstPart : map processPart otherParts

    -- Handle each part that came after [[
    processPart :: Text -> Text
    processPart part =
        case T.splitOn "]]" part of
            -- No ]] found - return [[ + part
            [partText] -> T.concat ["[[", partText]
            -- Found ]] - make link and keep rest
            linkText:rest ->
                T.concat [makeHtmlLink linkText linkText, T.concat rest]
            -- Empty case
            [] -> ""

-- | Generate an HTML link with slug-based href
makeHtmlLink :: Text  -- ^ Link text to display
             -> Text  -- ^ Target path/name to link to
             -> Text  -- ^ Generated HTML link
makeHtmlLink displayText targetName = T.concat
    [ "<a href=\"/notes/"
    , makeSlug targetName
    , "/\">"
    , displayText
    , "</a>"
    ]
