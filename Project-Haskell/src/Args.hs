module Args where

import Data.List
import Result
import Text.Read (readMaybe)

data Args = Args
  { argImageConfigFile :: Maybe String, -- ^ Given with the `-imageConfigFile` argument
    argSceneFile :: Maybe String, -- ^ Given with the `-sceneFile` argument
    argOutFile :: Maybe String, -- ^ Given with the `-outFile` argument
    argNrSamples :: Maybe Int -- ^ Given with the `-imageNrSamples` argument
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs

--helper function which the returns the elements from even positions to work with keys
getEvens:: [String] -> [String]
getEvens [] = []
getEvens [x] = [x]
getEvens (x:y:xs) = x: getEvens xs

toArgMap :: [String] -> Result ParseArgsError ArgMap
toArgMap [] = Success $ []
toArgMap list
  | odd(length list) == True = Error InvalidArgs
  | any (not . isPrefixOf "-") (getEvens list) == True = Error InvalidArgs
  | otherwise = Success $ Data.List.zip keyList valueList where 
      keyList = map (dropWhile (=='-')) $ filter (isPrefixOf "-") list
      valueList = filter(not . isPrefixOf "-") list
      

-- >>> getArg "key" [("key", "value")]
-- Just "value"
getArg :: String -> ArgMap -> Maybe String
getArg keyName list =
  case list of 
    [] -> Nothing
    (key, value) : restOfList ->
      if key == keyName 
        then 
          Just value
        else 
          getArg keyName restOfList 

-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg keyName list =
  case list of 
    [] -> Nothing
    (key, value) : restOfList ->
        if key == keyName 
        then 
          readMaybe value 
        else 
          readArg keyName restOfList 


-- >>> procArgs ["-imageNrSamples", "200", "-outFile", "image.bmp"]
-- Success (Args {argImageConfigFile = Nothing, argSceneFile = Nothing, argOutFile = Just "image.bmp", argNrSamples = Just 200})

procArgs :: [String] -> Result ParseArgsError Args
procArgs inputString = 
  let 
    auxiliaryList = toArgMap inputString
  in 
      case auxiliaryList of
        Error InvalidArgs -> Error InvalidArgs
        Success list -> let 
                          imageFile = getArg "imageConfigFile" list
                          sceneFile = getArg "sceneFile" list
                          outFile = getArg "outFile" list
                          imageNr = readArg "imageNrSamples" list
                        in
                          Success Args {argImageConfigFile = imageFile, argSceneFile = sceneFile, argOutFile= outFile, argNrSamples= imageNr}

