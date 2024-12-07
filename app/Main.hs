{-# LANGUAGE OverloadedStrings #-}

import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory )
import System.FilePath
    ( (</>), takeExtension, replaceExtension, takeBaseName )
import Control.Monad (unless, when)
import CMark (commonmarkToHtml)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
    let sourceDir = "notes"
    let destDir = "dist"

    sourceExists <- checkSourceDirectory sourceDir
    when sourceExists $ do
        createDestinationDirectory destDir
        mdFiles <- listMarkdownFiles sourceDir
        convertMarkdownFiles sourceDir destDir mdFiles

checkSourceDirectory :: FilePath -> IO Bool
checkSourceDirectory sourceDir = do
    sourceExists <- doesDirectoryExist sourceDir
    unless sourceExists $ do
        putStrLn $ "Error: Source directory '" ++ sourceDir ++ "' does not exist."
    return sourceExists

createDestinationDirectory :: FilePath -> IO ()
createDestinationDirectory destDir = createDirectoryIfMissing True destDir

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles sourceDir = do
    files <- listDirectory sourceDir
    return $ filter (\f -> takeExtension f == ".md") files

-- Add new type for backlinks mapping
type BacklinksMap = Map FilePath [FilePath]

-- Add function to create backlinks map from links map
createBacklinksMap :: WikiLinks -> BacklinksMap
createBacklinksMap linksMap = Map.fromListWith (++)
    [ (T.unpack linkTarget, [sourceFile])
    | (sourceFile, links) <- Map.toList linksMap
    , linkTarget <- links
    ]

-- Update convertMarkdownFiles to pass backlinks
convertMarkdownFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
convertMarkdownFiles sourceDir destDir mdFiles = do
    template <- TIO.readFile "app/template.html"
    
    -- Collect wiki links from all files
    linksMap <- collectWikiLinks sourceDir mdFiles
    
    -- Create backlinks map
    let backlinksMap = createBacklinksMap linksMap
    
    -- Debug print backlinks (temporary)
    mapM_ (\(file, backlinks) -> do
        putStrLn $ "Backlinks to " ++ file ++ ":"
        mapM_ putStrLn backlinks
        ) (Map.toList backlinksMap)
    
    -- Continue with existing conversion
    mapM_ (convertFile template sourceDir destDir backlinksMap) mdFiles
    putStrLn $ "Converted " ++ show (length mdFiles) ++ " markdown files to HTML"

-- Update convertFile signature to accept backlinks
convertFile :: T.Text -> FilePath -> FilePath -> BacklinksMap -> FilePath -> IO ()
convertFile template sourceDir destDir backlinksMap file = do
    let sourcePath = sourceDir </> file
    let destPath = replaceExtension (destDir </> file) ".html"
    markdown <- TIO.readFile sourcePath
    
    -- Debug print backlinks for current file
    let currentBacklinks = Map.findWithDefault [] file backlinksMap
    putStrLn $ "Converting " ++ file ++ " with backlinks:"
    mapM_ (\bl -> putStrLn $ "  - " ++ bl) currentBacklinks
    
    -- Existing conversion logic
    let body = replaceWikiLinks $ commonmarkToHtml [] markdown
    let title = T.pack (takeBaseName file)
    let html = T.replace "{{title}}" title $ T.replace "{{body}}" body template
    TIO.writeFile destPath html

replaceWikiLinks :: T.Text -> T.Text
replaceWikiLinks text = processText text
  where
    -- Break into parts and process each
    processText input = 
        case T.splitOn "[[" input of
            [] -> ""                        -- Handle empty list case
            [singlePart] -> singlePart
            -- Found [[ - process parts
            firstPart:otherParts -> 
                T.concat $ firstPart : map processPart otherParts

    -- Handle each part that came after [[
    processPart part =
        case T.splitOn "]]" part of
            -- No ]] found - return [[ + part
            [partText] -> T.concat ["[[", partText]
            -- Found ]] - make link and keep rest
            linkText:rest -> 
                T.concat [makeLink linkText, T.concat rest]
            -- Empty case
            [] -> ""

    -- Convert wiki text to HTML link
    makeLink link = T.concat ["<a href=\"", makeSlug link, "\">", link, "</a>"]

makeSlug :: Text -> Text
makeSlug = T.intercalate "-"                -- Join parts with hyphens
         . filter (not . T.null)            -- Remove empty parts
         . T.split (not . C.isAlphaNum)     -- Split on non-alphanumeric chars
         . T.toLower                        -- Convert to lowercase

-- Example usage:
-- makeSlug "Wiki Link!" -> "wiki-link"
-- makeLink "Wiki Link!" -> "<a href="wiki-link">Wiki Link!</a>"

extractWikiLinks :: T.Text -> [T.Text]
extractWikiLinks text = go text []
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

type WikiLinks = Map FilePath [T.Text]

collectWikiLinks :: FilePath -> [FilePath] -> IO WikiLinks
collectWikiLinks sourceDir files = do
    pairs <- mapM (\file -> do
        content <- TIO.readFile (sourceDir </> file)
        let links = extractWikiLinks content
        return (file, links)
        ) files
    return $ Map.fromList pairs
