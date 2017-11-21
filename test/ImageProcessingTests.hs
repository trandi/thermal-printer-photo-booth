module ImageProcessingTests where

import ImageProcessing
import Codec.Picture
import Test.HUnit.Base
import Test.QuickCheck.Modifiers

testImage = generateImage (\x y -> fromIntegral (x*y)) 5 4 :: Image Pixel8
testImageBW = generateImage (\x y -> if x*y < 3 then 0 else 255) 5 4 :: Image Pixel8

imageProcessingTests = 
    TestList [
        TestCase (assertEqual "Image AVG" 3 (greyImagePixelsAvg testImage)),
        TestCase (assertEqual "BW image" (imageData testImageBW) (imageData (greyToBW testImage)))
        ] 
        
        
prop_imageToStripes :: NonNegative Int -> NonNegative Int -> Bool
prop_imageToStripes (NonNegative w) (NonNegative h) =
    length stripes == ceiling (divAsFloat (imageHeight img) stripeHeight)
    where 
        img = generateImage (\i j -> fromIntegral (i*j)) w h
        stripes = imageToStripes img
