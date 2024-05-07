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
parseLibraryInput input   | lhs == "Book" = Book ((trim (drop 1 title)), (trim (drop 1 author)))
                          | lhs == "Author" = Author (trim (drop 1 rhs))
                          | lhs == "Title" = Title (trim (drop 1 rhs))
                          | elem (map toLower input) ["q", "e", "exit", "quit"] = Exit
                          | otherwise = (Error input)
                            where
                              (lhs, rhs) = span (/= ':') (trim input)
                              (title, author) = span (/= ';') (trim rhs)


-- exercise:
main :: IO ()
main = do
  -- task a)
  -- replace with implementation:
  return ()
  -- end replace

library :: [(String,String)] -> IO ()
library books =  do
  -- task c)
  -- replace with implementation:
  return ()
  -- end replace
      

getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
    -- task b)
  -- replace with implementation:
  return Exit
  -- end replace
