module Task2 where

import Data.Word

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Eq,Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Eq,Show,Read)

-- A)
getRed :: Rgb -> Word8
getRed (Rgb red green blue) = red

getGreen :: Rgb -> Word8
getGreen (Rgb red green blue) = green

getBlue :: Rgb -> Word8
getBlue (Rgb red green blue) = blue

grayscale :: Image -> Image
grayscale (Image 0 0 []) = (Image 0 0 [])
grayscale (Image width height content) = Image width height (map (\listOfPixels ->
                                                                 (map (\pixel -> 
	                        	                                      Rgb (createPixel pixel) 
                                                                      (createPixel pixel) 
                                                                      (createPixel pixel)) 
	                                                             listOfPixels)) 
                                                            content)
	          where createPixel p = round ((fromIntegral (getRed p)) * 0.30 + (fromIntegral (getGreen p)) * 0.59 + (fromIntegral (getBlue p)) * 0.11)

-- Б)
checkPixel :: [[Rgb]] -> Bool
checkPixel content = if (length (map (\listOfPixels -> 
	                                 (filter (\pixel -> 
	                                 	     (getRed pixel) == (getBlue pixel) && (getBlue pixel) == (getGreen pixel)) 
	                                 listOfPixels)) 
                                content) == 0) then False else True

isGrayscale :: Image -> Bool
isGrayscale (Image width height content) = if (checkPixel content == True) then True else False

kernel1 :: [[Float]]
kernel1 = [[1,0,-1],[2,0,-2], [1,0,-1]]

kernel2 :: [[Float]]
kernel2 = [[1,2,1],[0,0,0],[-1,-2,-1]]

-- convolution :: [[Float]] -> [[Float]] -> Float
-- convolution kernel matrix = 

  -- за всеки пиксел от img правя матрица 3х3 (edge handling extend) и правя конволюция с kernel1 и kernel2
  -- смятам sqrt (полученото от конволюцията с kernel1 на втора степен + полученото от конволюцията с kernel2 на втора степен)
  -- проверявам резултата в [0,255] ли е
  -- правя Rgb резултата резултата резултата
  -- изграждам ново изображение 

-- edgeDetect :: Image -> Image
-- edgeDetect img
--  | not $ isGrayscale img = error "edgeDetect: given image is not grayscale"
--  | img == (Image 0 0 []) = (Image 0 0 []) 
--  | otherwise = ...

-- В)
replaceElement :: [[Rgb]] -> Rgb -> Rgb -> (Int, Int) -> [[Rgb]]
replaceElement content targetColor replacementColor (x, y)
  | targetColor /= ((content !! x) !! y) = content
  | otherwise = take x content ++ [take y (content !! x) ++ [replacementColor] ++ drop (y + 1) (content !! x)] ++ drop (x + 1) content

floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill _ _ _ (Image 0 0 []) = (Image 0 0 [])
floodFill color x y img@(Image width height content)
    | x < 0 || y < 0 || x > (height - 1) || y > (width - 1) = img -- coord validation
    | otherwise = floodFillHelper ((content !! x) !! y) color x y img
 
 -- helper because we need stored target color
floodFillHelper :: Rgb-> Rgb -> Int -> Int -> Image -> Image
floodFillHelper targetColor replacementColor x y img@(Image width height content)
  | x < 0 || y < 0 || x > (height - 1) || y > (width - 1) = img - coord validation
  | replacementColor == ((content !! x) !! y) = img -- edge case 1
  | targetColor /= ((content !! x) !! y) = img -- edge case 2
  | otherwise = floodFillHelper targetColor replacementColor x (y - 1)
                    (floodFillHelper targetColor replacementColor x (y + 1)
                        (floodFillHelper targetColor replacementColor (x - 1) y
                            (floodFillHelper targetColor replacementColor (x + 1) y createImage)))
      where createImage = (Image width height (replaceElement content targetColor replacementColor (x, y)))

-- Г)
-- saveImage :: FilePath -> Image -> IO()
-- savaImage path img

-- Д)
-- loadImage :: String -> IO Image
-- loadImage path