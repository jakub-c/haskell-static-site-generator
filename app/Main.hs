{-# LANGUAGE OverloadedStrings #-}

import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory,
      copyFile,
      removePathForcibly )
import System.FilePath
    ( (</>), takeExtension, takeBaseName, takeDirectory )
import Control.Monad (unless, when, forM_)
import CMark (commonmarkToHtml, optUnsafe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

-- Configuration record for all directory paths
data SiteConfig = SiteConfig
    { sourceDir :: FilePath
    , notesDir  :: FilePath  
    , publicDir :: FilePath
    , staticDir :: FilePath
    } deriving (Show)

-- Smart constructor that ensures notesDir is derived from sourceDir
-- mk prefix stands for "make" and is a common convention in Haskell
mkSiteConfig :: FilePath -> FilePath -> FilePath -> SiteConfig
mkSiteConfig source public static = SiteConfig
    { sourceDir = source
    , notesDir  = source </> "notes"
    , publicDir = public  
    , staticDir = static
    }

main :: IO ()
main = do
    let config = mkSiteConfig "pages" "public" "static"

    -- Clear public directory first
    clearDistDirectory (publicDir config)

    sourceExists <- checkSourceDirectory (sourceDir config)
    when sourceExists $ do
        createDestinationDirectory (publicDir config)
        copyStaticFiles (staticDir config) (publicDir config)

        -- Load templates
        templateNotes <- TIO.readFile "app/template-notes.html"
        templateStatic <- TIO.readFile "app/template-static.html"

        -- Process root files with static template
        processRootFiles (sourceDir config) (publicDir config) templateStatic
        
        notesExists <- doesDirectoryExist (notesDir config)
        when notesExists $ do
            mdFiles <- listMarkdownFiles (notesDir config)
            convertMarkdownFiles (sourceDir config) (publicDir config) templateNotes templateStatic mdFiles

checkSourceDirectory :: FilePath -> IO Bool
checkSourceDirectory sourceDir = do
    sourceExists <- doesDirectoryExist (sourceDir)
    unless sourceExists $ do
        putStrLn $ "Error: Source directory '" ++ sourceDir ++ "' does not exist."
    return sourceExists

createDestinationDirectory :: FilePath -> IO ()
createDestinationDirectory publicDir = createDirectoryIfMissing True publicDir

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
getDestFilePath publicDir filename = 
    if filename == "00 - index.md"
    then publicDir </> "notes" </> "index.html"
    else publicDir </> "notes" </> slugFromFileName filename </> "index.html"

-- Update convertMarkdownFiles to pass backlinks
convertMarkdownFiles :: FilePath -> FilePath -> Text -> Text -> [FilePath] -> IO ()
convertMarkdownFiles sourceDir publicDir templateNotes templateStatic mdFiles = do
    createDirectoryIfMissing True (publicDir </> "notes")
    linksMap <- collectWikiLinks (sourceDir </> "notes") mdFiles
    let backlinksMap = createBacklinksMap linksMap

    forM_ mdFiles $ \file -> do
        let sourcePath = getSourceFilePath sourceDir file
        let destPath = getDestFilePath publicDir file
        createDirectoryIfMissing True (takeDirectory destPath)
        let template = if file == "00 - index.md" then templateStatic else templateNotes
        convertFile template sourcePath destPath backlinksMap file

    putStrLn $ "Converted " ++ show (length mdFiles) ++ " markdown files to HTML"

-- Update convertFile signature to accept backlinks
convertFile :: T.Text -> FilePath -> FilePath -> BacklinksMap -> FilePath -> IO ()
convertFile template sourcePath destPath backlinksMap _filename = do
    -- Read Markdown Content
    markdown <- TIO.readFile sourcePath

    -- Extract Base Name
    let baseName = takeBaseName sourcePath

    -- Find Backlinks for this file
    let currentBacklinks = Map.findWithDefault [] baseName backlinksMap

    -- Generate Backlinks HTML
    let backlinksHtml = T.concat 
            [ "<li><a href=\"/notes/"
            <> makeSlug (T.pack (takeBaseName bl))
            <> "/\">"
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

replaceWikiLinks :: T.Text -> T.Text
replaceWikiLinks text = processText text
  where
    -- Break into parts and process each
    processText :: T.Text -> T.Text
    processText input =
        case T.splitOn "[[" input of
            [] -> ""                        -- Handle empty list case
            [singlePart] -> singlePart
            -- Found [[ - process parts
            firstPart:otherParts ->
                T.concat $ firstPart : map processPart otherParts

    -- Handle each part that came after [[
    processPart :: T.Text -> T.Text
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
    makeLink :: T.Text -> T.Text
    makeLink linkText = makeHtmlLink linkText linkText

makeSlug :: Text -> Text
makeSlug = T.intercalate "-"                -- Join parts with hyphens
         . filter (not . T.null)            -- Remove empty parts
         . T.split (not . C.isAlphaNum)     -- Split on non-alphanumeric chars
         . T.toLower                        -- Convert to lowercase

-- Example usage:
-- makeSlug "Wiki Link!" -> "wiki-link"
-- makeLink "Wiki Link!" -> "<a href="wiki-link">Wiki Link!</a>"

-- Helper to generate slug from a file name
slugFromFileName :: FilePath -> String
slugFromFileName = T.unpack . makeSlug . T.pack . takeBaseName

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
    , "/\">"
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
processRootFiles sourceDir publicDir template = do
    -- Get all .md files from root, excluding notes directory
    files <- listDirectory sourceDir
    let rootMdFiles = filter (\f -> takeExtension f == ".md") files

    -- Process each file
    forM_ rootMdFiles $ \file -> do
        let sourcePath = sourceDir </> file
        let destPath = publicDir </> slugFromFileName file <> ".html"

        content <- TIO.readFile sourcePath
        let body = commonmarkToHtml [optUnsafe] content
        let title = T.pack $ takeBaseName file
        let html = T.replace "{{title}}" title 
                $ T.replace "{{body}}" body template

        TIO.writeFile destPath html
