import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory )
import System.FilePath ( (</>), takeExtension, replaceExtension )
import Control.Monad (unless, when)
import CMark (commonmarkToHtml)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

convertMarkdownFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
convertMarkdownFiles sourceDir destDir mdFiles = do
    mapM_ convertFile mdFiles
    putStrLn $ "Converted " ++ show (length mdFiles) ++ " markdown files to HTML"
  where
    convertFile file = do
        let sourcePath = sourceDir </> file
        let destPath = replaceExtension (destDir </> file) ".html"
        markdown <- TIO.readFile sourcePath
        let html = commonmarkToHtml [] markdown
        TIO.writeFile destPath html