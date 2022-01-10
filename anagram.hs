import Data.List
import Data.Char (isAlpha)

main :: IO ()
main = do
  putStrLn "Enter the first word"
  word1 <- getLine
  putStrLn "Enter the second word"
  word2 <- getLine
  print (areAnagrams word1 word2)


isAnagram :: String -> String -> Bool
isAnagram word1 word2 = (sort word1) == (sort word2)

isWord :: String -> Maybe String
isWord word = 
  case null word of 
    True -> Nothing
    False -> case (all isAlpha word) of
               False -> Nothing
               True -> Just word

areAnagrams :: String -> String -> String
areAnagrams word1 word2 =
  case isWord word1 of
    Nothing -> "First word is invalid"
    Just word1 -> case isWord word2 of 
                    Nothing -> "Second word is invalid"
                    Just word1 -> case (isAnagram word1 word2) of
                                    False -> "Second word is not an anagram of the first word"
                                    True -> "Anagrams!"

               
