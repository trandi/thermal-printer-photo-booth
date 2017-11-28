-- pure functions dealing with the Image splitting into stripes, conversions, etc. ...
module ImageProcessing where

import Codec.Picture
import Codec.Picture.Extra
import Data.Foldable


scaledWidth = 240
stripeHeight = 8


-- split the image into 8dots high bands / stripes
imageToStripes :: Image Pixel8 -> [Image Pixel8]
imageToStripes img@(Image width height _) = 
    fmap (\row -> crop 0 row width stripeHeight img) [0, stripeHeight..(height-1)]




-- we take any image, but need to scale it to fit on the paper, make it grey and then Black&White
transformImage :: Bool -> DynamicImage -> Image Pixel8
transformImage scale = greyToBW . imageRGB8toGrey . (if scale then scaleImage else id ) . convertRGB8


scaleImage ::  Image PixelRGB8 -> Image PixelRGB8
scaleImage img = scaleBilinear width height img
    where width = scaledWidth
          -- keep ratio, but dots are taller than they are wide, hence the empiric 0.7 coefficient
          height = round ((fromIntegral (imageHeight img)) * (divAsFloat width (imageWidth img)) * 0.7) 

divAsFloat :: Int -> Int -> Float
divAsFloat a b = (fromIntegral a) / (fromIntegral b)


imageRGB8toGrey :: Image PixelRGB8 -> Image Pixel8
imageRGB8toGrey = pixelMap pixelRGB8toGrey

-- transforms RGB to grey by simply taking the average of the 3 colour channels
pixelRGB8toGrey :: PixelRGB8 -> Pixel8
pixelRGB8toGrey (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3 + (r `mod` 3 + g `mod` 3 + b `mod` 3) `div` 3)

-- Compare each pixel with the average of the image. If below -> it becomes ablack dot (0), otherwise white (255)
greyToBW :: Image Pixel8 -> Image Pixel8
greyToBW img = 
    pixelMap (\pixel -> if pixel < avgPixel then 0 else 255) img
    where avgPixel = greyImagePixelsAvg img


-- using Integer to avoid arithmetic overflows when summing up all the pixels in the image
greyImagePixelsAvg :: Image Pixel8 -> Pixel8
greyImagePixelsAvg img@(Image width height _) = 
    fromIntegral (totalSum `div` fromIntegral (width * height))
    where 
        allImgValues = [toInteger (pixelAt img i j) | i <- [0..(width-1)], j <- [0..(height-1)]]
        totalSum = sum allImgValues :: Integer
