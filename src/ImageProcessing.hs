-- pure functions dealing with the Image splitting into stripes, conversions, etc. ...
module ImageProcessing where

import Codec.Picture
import Codec.Picture.Extra
import Data.Foldable


-- split the image into 8dots high bands / stripes
imageToStripes :: Image Pixel8 -> [Image Pixel8]
imageToStripes img@(Image width height _) = 
    fmap (\row -> crop 0 row width 8 img) [0, 8..(height-1)]




-- we take any image, but need to scale it to fit on the paper, make it grey and then Black&White
transformImage :: DynamicImage -> Image Pixel8
transformImage = greyToBW . imageRGB8toGrey . scaleImage


scaleImage ::  DynamicImage -> Image PixelRGB8
scaleImage img = scaleBilinear width height rgb8Img
    where rgb8Img = convertRGB8 img
          width = 240
          -- keep ratio, but the dots are taller in size than they are wide, hence the empiric 0.7 coefficient
          height = round ((fromIntegral (imageHeight rgb8Img)) * (divAsFloat width (imageWidth rgb8Img)) * 0.7) 

divAsFloat :: Int -> Int -> Float
divAsFloat a b = (fromIntegral a) / (fromIntegral b)


imageRGB8toGrey :: Image PixelRGB8 -> Image Pixel8
imageRGB8toGrey = pixelMap pixelRGB8toGrey

-- transforms RGB to grey by simply taking the average of the 3 colour channels
pixelRGB8toGrey :: PixelRGB8 -> Pixel8
pixelRGB8toGrey (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3 + (r `mod` 3 + g `mod` 3 + b `mod` 3) `div` 3)

-- Compare each pixel with the average of the image. If below -> it becomes ablack dot (0), otherwise white (255)
greyToBW :: Image Pixel8 -> Image Pixel8
greyToBW img = pixelMap (\pixel -> if pixel < avgPixel then 0 else 255) img
    where avgPixel = greyImagePixelsAvg img


-- using Integer to avoid arithmetic overflows when summing up all the pixels in the image
greyImagePixelsAvg :: Image Pixel8 -> Pixel8
greyImagePixelsAvg img@(Image width height _) = 
    fromIntegral (totalSum `div` fromIntegral (width * height))
    where 
        allIndices = [(i, j) | i <- [0..(width-1)], j <- [0..(height-1)]]
        totalSum = foldl' (\acc (i, j) -> acc + fromIntegral (pixelAt img i j)) 0 allIndices :: Integer
