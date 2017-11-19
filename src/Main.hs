module Main where

-- reading / manipulating images (hip package)
import Codec.Picture
import Codec.Picture.Extra

-- for sending serial data to the printer (serialport package)
import Data.Char
import Data.Bits
import Data.Word
import Data.Foldable
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B

main = do
    eitherImg <- readImage "test.jpg"
    case eitherImg of
        Left msg -> print msg
        Right img -> do
            let printableImg = transformImage img
            savePngImage "test-transformed.png" (ImageY8 printableImg)
            sendImgToPrinter printableImg
            print "Done"




port = "/dev/ttyUSB0"

sendImgToPrinter :: Image Pixel8 -> IO ()
sendImgToPrinter img = do
    port <- openSerial port defaultSerialSettings {commSpeed = CS19200}
    
    let stripes = imageToStripes img :: [Image Pixel8]
    
    let stripesBytes = fmap (\stripe -> stripeToBytes stripe) stripes :: [[Char]]
    let stripesPrinterStrings = fmap (\stripeBytes -> stripeBytesToPrinterString stripeBytes ) stripesBytes :: [String]
    -- below will actuall write to the serial port
    let totalBytesSentIO = foldl' (sendStringToPrinter port) (return 0) stripesPrinterStrings

    totalBytesSent <- totalBytesSentIO
    print ("Sent " ++ (show totalBytesSent) ++ " bytes to the printer.")



sendStringToPrinter :: SerialPort -> IO Int -> String -> IO Int
sendStringToPrinter port bytesSentUntilNow printerStr = 
    fmap sum (sequence [bytesSentUntilNow, bytesSent])
    where
        bytesSent = send port (B.pack printerStr)
    

-- split the image into 8dots high bands / stripes
imageToStripes :: Image Pixel8 -> [Image Pixel8]
imageToStripes img@(Image width height _) = 
    fmap (\row -> crop 0 row width 8 img) [0, 8..(height-1)]

-- the thermal printer wants a stripe to be send as 1 command (ie between ESC* and NewLine)
stripeToBytes :: Image Pixel8 -> [Char]
stripeToBytes img@(Image width height _) =
    fmap ( chr . column8PixelsToPrinterChar . (extractColumn img height)) [0..(width-1)]

extractColumn :: Image Pixel8 -> Int -> Int -> [Pixel8]
extractColumn img height col = fmap (\row -> pixelAt img col row) [0..(height - 1)]

column8PixelsToPrinterChar :: [Pixel8] -> Int
column8PixelsToPrinterChar pixels = 
    foldl' (\acc (index, pixelValue) -> acc + if isBlack pixelValue then 2^index else 0 ) 0 pixelsWithIndices
    where
        pixelsWithIndices = zip [0..] (reverse pixels) :: [(Int, Pixel8)]
        


-- we take any image, but need to scale it to fit on the paper, make it grey and then Black&White
transformImage :: DynamicImage -> Image Pixel8
transformImage = greyToBW . rgb8ToGrey . scaleImage


scaleImage ::  DynamicImage -> Image PixelRGB8
scaleImage img = scaleBilinear width height rgb8Img
    where rgb8Img = convertRGB8 img
          width = 180
          height = (imageHeight rgb8Img) * width `div` (imageWidth rgb8Img) -- keep ratio


rgb8ToGrey :: Image PixelRGB8 -> Image Pixel8
rgb8ToGrey = pixelMap pixelAvg

pixelAvg :: PixelRGB8 -> Pixel8
pixelAvg (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3 + (r `mod` 3 + g `mod` 3 + b `mod` 3) `div` 3)

greyToBW :: Image Pixel8 -> Image Pixel8
greyToBW = pixelMap blackOrWhite

blackOrWhite :: Pixel8 -> Pixel8
blackOrWhite value
    | isBlack value = 0
    | otherwise = 255


isBlack :: Pixel8 -> Bool
isBlack = (<128)




sel_bit_img = [chr 0x1B, chr 0x2A, chr 0x00]  -- ESC*0
new_line = [chr 0x0A]
-- each element represents 8 vertical bits where black dots are 1 and white are 0
-- basically it's 1 column in the 8 bits wide horisontal stripe
stripeBytesToPrinterString :: [Char] -> String
stripeBytesToPrinterString dataBytes= sel_bit_img ++ (generateNlNh dataBytes) ++ dataBytes ++ new_line

-- this is simply the size of the provided array, written on 2 bytes, the Low and High (in this order)
generateNlNh :: [Char] -> [Char]
generateNlNh list = [chr (l .&. 0xFF), chr ((shift l (-8)) .&. 0xFF)]
    where l = length list
