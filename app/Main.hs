import System.Directory (createDirectoryIfMissing, listDirectory, copyFile, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import Control.Monad (when, unless)

main :: IO ()
main = do
    copyMdFiles

copyMdFiles :: IO ()
copyMdFiles = do
    let sourceDir = "notes"
    let destDir = "dist"
    
    -- Check if source directory exists
    sourceExists <- doesDirectoryExist sourceDir
    unless sourceExists $ do
        putStrLn $ "Error: Source directory '" ++ sourceDir ++ "' does not exist."
        return ()

    -- If source directory exists, proceed with copying
    when sourceExists $ do
        -- Create destination directory if it doesn't exist
        createDirectoryIfMissing True destDir
        
        -- Get list of files in source directory
        files <- listDirectory sourceDir
        
        -- Filter for .md files and copy them
        let mdFiles = filter (\f -> takeExtension f == ".md") files
        mapM_ (\file -> copyFile (sourceDir </> file) (destDir </> file)) mdFiles
        
        putStrLn $ "Copied " ++ show (length mdFiles) ++ " .md files from " ++ sourceDir ++ " to " ++ destDir