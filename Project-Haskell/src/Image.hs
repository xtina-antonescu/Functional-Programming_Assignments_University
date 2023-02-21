module Image where

import Parser
import Result
import Scene.Loader
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Prelude hiding (readFile)

data Image = Image
  { imageAspectRatio :: Double,
    imageWidth :: Int,
    imageHeight :: Int,
    imageNrSamples :: Int,
    imageMaxDepth :: Int
  }
  deriving (Eq, Show)

setupImage :: Int -> Int -> Int -> Int -> Image
setupImage w h nrSamples maxDepth =
  Image
    { imageAspectRatio = fromIntegral w / fromIntegral h,
      imageWidth = w,
      imageHeight = h,
      imageNrSamples = nrSamples,
      imageMaxDepth = maxDepth
    }

defaultImage :: Image
defaultImage = setupImage 800 450 100 50


-- >>> runParser imageParser "image {width 800 height 450 nr_samples 100 max_depth 50}"
-- Success (Image {imageAspectRatio = 1.7777777777777777, imageWidth = 800, imageHeight = 450, imageNrSamples = 100, imageMaxDepth = 50},"")
imageParser :: Parser Image
imageParser = pMap (\x -> snd x) $ string "image " `andThen` dict4Parser setupImage ("width" , number) ("height" , number) ("nr_samples", number) ("max_depth" , number)


getImageConfig :: TestableMonadIO io => Maybe String -> io (Result LoadingError Image)
getImageConfig maybePath =
  case maybePath of
    Just path -> loadImageConfig path
    Nothing -> return $ Success defaultImage

loadImageConfig :: TestableMonadIO io => String -> io (Result LoadingError Image)
loadImageConfig path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      fileContents <- readFile path
      case parse imageParser fileContents of
        Success image -> return $ Success image
        Error parseError -> return $ Error (ParseFailed parseError)
    else return $ Error FileNotFound