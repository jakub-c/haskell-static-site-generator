import System.Directory
    ( copyFile,
      createDirectoryIfMissing,
      doesDirectoryExist,
      listDirectory )
import System.FilePath ( (</>), takeExtension )
import Control.Monad (unless, when)

main :: IO ()
main = do
    let sourceDir = "notes"
    let destDir = "dist"
    
    sourceExists <- checkSourceDirectory sourceDir
    when sourceExists $ do
        createDestinationDirectory destDir
        mdFiles <- listMarkdownFiles sourceDir
        copyMarkdownFiles sourceDir destDir mdFiles

checkSourceDirectory :: FilePath -> IO Bool
checkSourceDirectory sourceDir = do
    sourceExists <- doesDirectoryExist sourceDir
    unless sourceExists $ do
        putStrLn $ "Error: Source directory '" ++ sourceDir ++ "' does not exist."
    return sourceExists

createDestinationDirectory :: FilePath -> IO ()
createDestinationDirectory destDir = do
    createDirectoryIfMissing True destDir

listMarkdownFiles :: FilePath -> IO [FilePath]
listMarkdownFiles sourceDir = do
    files <- listDirectory sourceDir
    return $ filter (\f -> takeExtension f == ".md") files

copyMarkdownFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyMarkdownFiles sourceDir destDir mdFiles = do
    mapM_ (\file -> copyFile (sourceDir </> file) (destDir </> file)) mdFiles
    putStrLn $ "Copied " ++ show (length mdFiles) ++ " .md files from " ++ sourceDir ++ " to " ++ destDir