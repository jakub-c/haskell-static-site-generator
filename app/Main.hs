{-# LANGUAGE OverloadedStrings #-}

import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory,
      copyFile,
      removePathForcibly )
import System.FilePath
    ( (</>), takeExtension, takeBaseName, takeDirectory, replaceExtension )
import Control.Monad (unless, when, forM_)
import CMark (commonmarkToHtml, optUnsafe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
    let sourceDir = "pages"
    let notesDir = sourceDir </> "notes"
    let destDir = "dist"
    let staticDir = "static"

    -- Clear dist directory first
    clearDistDirectory destDir

    sourceExists <- checkSourceDirectory sourceDir
    when sourceExists $ do
        createDestinationDirectory destDir
        copyStaticFiles staticDir destDir

        -- Load templates
        templateNotes <- TIO.readFile "app/template-notes.html"
        templateStatic <- TIO.readFile "app/template-static.html"

        -- Process root files with static template
        processRootFiles sourceDir destDir templateStatic
        
        notesExists <- doesDirectoryExist notesDir
        when notesExists $ do
            mdFiles <- listMarkdownFiles notesDir
            convertMarkdownFiles sourceDir destDir templateNotes templateStatic mdFiles

checkSourceDirectory :: FilePath -> IO Bool
checkSourceDirectory sourceDir = do
    sourceExists <- doesDirectoryExist (sourceDir)
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

-- Helper function to get source file path
getSourceFilePath :: FilePath -> FilePath -> FilePath
getSourceFilePath sourceDir filename = sourceDir </> "notes" </> filename

-- Helper function to get destination file path
getDestFilePath :: FilePath -> FilePath -> FilePath
getDestFilePath destDir filename = 
    if filename == "00 - index.md"
    then destDir </> "notes" </> "index.html"
    else destDir </> "notes" </> slugName </> "index.html"
    where slugName = T.unpack $ makeSlug $ T.pack $ takeBaseName filename

-- Update convertMarkdownFiles to pass backlinks
convertMarkdownFiles :: FilePath -> FilePath -> Text -> Text -> [FilePath] -> IO ()
convertMarkdownFiles sourceDir destDir templateNotes templateStatic mdFiles = do
    createDirectoryIfMissing True (destDir </> "notes")
    linksMap <- collectWikiLinks (sourceDir </> "notes") mdFiles
    let backlinksMap = createBacklinksMap linksMap
    
    mapM_ (\file -> do
        let sourcePath = getSourceFilePath sourceDir file
        let destPath = getDestFilePath destDir file
        createDirectoryIfMissing True (takeDirectory destPath)
        let template = if file == "00 - index.md" then templateStatic else templateNotes
        convertFile template sourcePath destPath backlinksMap file
        ) mdFiles
    
    putStrLn $ "Converted " ++ show (length mdFiles) ++ " markdown files to HTML"

-- Update convertFile signature to accept backlinks
convertFile :: T.Text -> FilePath -> FilePath -> BacklinksMap -> FilePath -> IO ()
convertFile template sourcePath destPath backlinksMap filename = do
    -- Read Markdown Content
    markdown <- TIO.readFile sourcePath

    -- Extract Base Name
    let baseName = takeBaseName sourcePath

    -- Find Backlinks for this file
    let currentBacklinks = Map.findWithDefault [] baseName backlinksMap

    -- Generate Backlinks HTML
    let backlinksHtml = T.concat 
            [ "<li><a href=\"/notes/"
            <> makeSlug (T.pack bl)
            <> "\">"
            <> T.pack (takeBaseName bl)
            <> "</a></li>\n"
            | bl <- currentBacklinks
            ]

    -- Convert Markdown to HTML and Replace Wiki Links
    let body = replaceWikiLinks $ commonmarkToHtml [optUnsafe] markdown

    -- Generate Title
    let title = T.pack baseName

    -- Replace Placeholders in Template
    let html = T.replace "{{title}}" title 
             $ T.replace "{{body}}" body 
             $ T.replace "{{backlinks}}" backlinksHtml template

    -- Ensure Destination Directory Exists
    createDirectoryIfMissing True (takeDirectory destPath)

    -- Write the HTML File
    TIO.writeFile destPath html

createDestPath :: FilePath -> FilePath -> FilePath -> IO FilePath
createDestPath destDir baseName _ =
    if baseName == "00 - index"
    then return $ destDir </> "index.html"
    else do
        let noteDir = destDir </> baseName
        createDirectoryIfMissing True noteDir
        return $ noteDir </> "index.html"

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
    makeLink linkText = makeHtmlLink linkText linkText

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

-- Add this function at module level
makeHtmlLink :: T.Text  -- ^ Link text to display
             -> T.Text  -- ^ Target path/name to link to
             -> T.Text  -- ^ Generated HTML link
makeHtmlLink displayText targetName = T.concat
    [ "<a href=\"/notes/"
    , makeSlug targetName
    , "\">"
    , displayText
    , "</a>"
    ]

-- Add this function to copy static files
copyStaticFiles :: FilePath -> FilePath -> IO ()
copyStaticFiles sourceDir destDir = do
    exists <- doesDirectoryExist sourceDir
    when exists $ do
        files <- listDirectory sourceDir
        forM_ files $ \file -> do 
            let sourcePath = sourceDir </> file
            let destPath = destDir </> file
            copyFile sourcePath destPath
        putStrLn $ "Copied static files to " ++ destDir
-- Add function to clear dist directory
clearDistDirectory :: FilePath -> IO ()
clearDistDirectory dir = do
    exists <- doesDirectoryExist dir
    when exists $ removePathForcibly dir

-- Add new helper function
processRootFiles :: FilePath -> FilePath -> Text -> IO ()
processRootFiles sourceDir destDir template = do
    -- Get all .md files from root, excluding notes directory
    files <- listDirectory sourceDir
    let rootMdFiles = filter (\f -> takeExtension f == ".md") files
    
    -- Process each file
    forM_ rootMdFiles $ \file -> do
        let sourcePath = sourceDir </> file
        let slugName = T.unpack $ makeSlug $ T.pack $ takeBaseName file
        let destPath = destDir </> slugName <> ".html"
        
        content <- TIO.readFile sourcePath
        let body = commonmarkToHtml [optUnsafe] content
        let title = T.pack $ takeBaseName file
        let html = T.replace "{{title}}" title 
                $ T.replace "{{body}}" body template
        
        TIO.writeFile destPath html
