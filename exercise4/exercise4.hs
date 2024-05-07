{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Data.Char

data LibraryInput = Exit | Error String | Book (String, String) | Author String | Title String

instance Show LibraryInput where
  show Exit = "Exit"
  show (Error xs) = "Invalid Input: " ++ xs
  show (Book (title, author)) = "Book: " ++ title ++ ";" ++ author
  show (Author author) = "Author: " ++ author
  show (Title title) = "Title: " ++ title

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseLibraryInput :: String -> LibraryInput
parseLibraryInput input   | lhs == "Book" = Book (trim (drop 1 title), trim (drop 1 author))
                          | lhs == "Author" = Author (trim (drop 1 rhs))
                          | lhs == "Title" = Title (trim (drop 1 rhs))
                          | map toLower input `elem` ["q", "e", "exit", "quit"] = Exit
                          | otherwise = Error input
                            where
                              (lhs, rhs) = span (/= ':') (trim input)
                              (title, author) = span (/= ';') (trim rhs)


-- exercise:
main :: IO ()
main = do
  -- task a)
  -- replace with implementation:
  putStrLn "Welcome to your Library"
  library []
  putStrLn "Bye!"
  -- end replace

library :: [(String,String)] -> IO ()
library books =  do
  -- task c)
  -- replace with implementation:
  input <- getInput
  case input of
    Exit -> return ()
    Error msg -> do
      putStrLn $ "There has been an error: " ++ msg
      library books
    Book (title, author) -> do
      -- putStrLn $ show input
      putStrLn "Do you want to (p)ut the book back or do you want to (t)ake the book?"
      choice <- getLine
      case choice of
        "p" -> do
          putStrLn "Done!"
          library ((title, author) : books)
        "t" -> do
          if (title, author) `elem` books
            then do
              putStrLn "Done!"
              let updatedBooks = filter (/= (title, author)) books
              library updatedBooks
            else do
              putStrLn "You do not have this book!"
              library books
        _ -> do
          putStrLn "Wrong input!"
          library books
    Author author -> do
      -- putStrLn $ show input
      let filteredBooks = filter (\(_, a) -> a == author) books
      putStrLn $ "You have the following books from " ++ author
      putStrLn $ unlines $ map (\(t, _) -> "Book: " ++ t ++ "; " ++ author) filteredBooks
      library books
    Title title -> do
      -- putStrLn $ show input
      let filteredBooks = filter (\(t, _) -> t == title) books
      putStrLn $ "You have the following books with the title: " ++ title
      putStrLn $ unlines $ map (\(t, a) -> "Book: " ++ t ++ "; " ++ a) filteredBooks
      library books
  -- end replace


getInput :: IO LibraryInput
getInput = do
    -- task b)
    putStrLn "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
        -- task b)
    -- replace with implementation:
    putStrLn "> "
    input <- getLine
    return (parseLibraryInput input)
    -- return Exit
    -- end replace


-- task d
-- It is impossible to write a function: f :: IO () -> Int when usage of predefined functions which are not included in Prelude is not allowed.
-- So the function main' :: Int can not be implemented
