module Main where
import Data.Char (isSpace)
-- Here we introduce the <$> operator
-- for its implementation try out ":t (<$>)" in your terminal. 
-- As you can see its very similar to what we saw earlier with "fmap"
-- this operator will be particularly convenient for dealing with our new
-- data types when dealing with inputs from users.
main :: IO ()
main = do
  putStrLn "Please enter your username"
  usernm <- Username <$> getLine
  putStrLn "Please enter your password"
  pass <- Password <$> getLine
  print (makeUser usernm pass)

-- We define the new types that will help us have code with much better readability.
newtype Password = 
  Password String deriving (Eq, Show)

-- The expression above is a type declaration, note that the word "Password"
-- appears twice. First it represents the name of the new type and then the 
-- name for the type constructor. Type ":t Password" in ghci, what does the
-- output mean?
--
-- It's not necessary for the type and constructor to have the same name
-- but it will be much easier to remember, if you find it confusing another common way to do this is "newtype Password = myPassword String ...to have the same name
-- but it will be much easier to remember, if you find it confusing another common way to do this is "newtype Password = myPassword String ...""


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

makeUser :: Username -> Password -> Either Error User
makeUser name pass = 
  User <$> validateUsername name
  <*> validatePassword password
