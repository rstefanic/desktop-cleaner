module Main where

import System.FilePath.Windows
import System.Environment
import System.Directory
import System.Exit (exitFailure)
import Data.Time

main :: IO ()
main = do
    args <- getArgs

    case args of
      ["--help"]   -> help
      ["--clean"]  -> cleanDesktop False
      ["--delete"] -> cleanDesktop True
      _            -> cleanDesktop False

help :: IO ()
help = do
    progName <- getProgName
    putStr . unlines $ 
        concat ["\nProgram Usage: ", progName, " [OPTION]"] :
        "\nOptions:\n" :
        "--clean   [Default Option]           Puts the extra files on your desktop into a folder" :
        "--delete                             Deletes the extra files on your Desktop" :
        "--help                               Display this message\n" :
        []

cleanDesktop :: Bool -> IO ()
cleanDesktop deleteFolder = do
    desktopFiles <- desktopPath >>= listDirectory
    createFileIfNonexistent
    mapM_ moveFile desktopFiles
    newFile <- desktopPath >>= \x -> getDate >>= \y -> return $ x </> y
    case deleteFolder of
      True  -> do
        removeDirectoryRecursive newFile
        putStrLn $ "Extra desktop files removed."
      False -> putStrLn $ "Files moved to " ++ newFile

getDate :: IO String
getDate = getCurrentTime >>= (\x -> return $ (show . utctDay) x )

desktopPath :: IO FilePath
desktopPath = getHomeDirectory >>= \path -> return $ path </> "Desktop"

createFileIfNonexistent :: IO ()
createFileIfNonexistent = do
    date <- getDate
    newFile <- desktopPath >>= \path -> return $ path </> date
    createDirectoryIfMissing False newFile

moveFile :: FilePath -> IO ()
moveFile file = do
    fileExists <- desktopPath >>= \desktop -> doesFileExist (desktop </> file)
    case (fileExists) of
        True  -> oldPath >>= \old -> newPath >>= \new -> renameFile old new
        False -> return ()
    where
        newFileName = getDate
        oldPath = desktopPath >>= \path -> return $ path </> file
        newPath = desktopPath >>= \path -> newFileName >>= \newFilePath -> 
                                return $ path </> newFilePath </> file