module Main where
import Data.Char (isSpace)

main :: IO ()
main = do
  putStrLn "Please enter your username"
  usernm <- Username <$> getLine
  putStrLn "Please enter your password"
  pass <- Password <$> getLine
  print (makeUser usernm pass)


newtype Password = 
  Password String deriving (Eq, Show)

newtype Error =
  Error String deriving (Eq, Show)

newtype Username =
  Username String deriving (Eq, Show)

data User =
  User Username Password deriving (Eq, Show)


maxLength :: String -> Either Error Password
maxLength x = case (length x > 20) of
  True -> Left (Error "Password must have at most 20 characters")
  False -> Right (Password x)

usernameLength :: String -> Either Error Username
usernameLength x = case (length x > 20) of
  True -> Left (Error "Username must have at most 20 characters")
  False -> Right (Username x)


checkSpaces :: String -> Either Error String 
checkSpaces "" = Left (Error "Password cannot be empty" )
checkSpaces (x:xs) = case (isSpace x) of 
  True -> checkSpaces xs
  False -> Right (x:xs)

validatePassword :: Password -> Either Error Password 
validatePassword (Password password) = 
  checkSpaces password >>= maxLength 

validateUsername :: Username -> Either Error Username
validateUsername (Username username) = 
  checkSpaces username >>= usernameLength

makeUser :: Username Password -> Either Error User
makeUser name pass = 
  User <$> validateUsername name
  <*> validatePassword
