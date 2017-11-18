module Main where

-- reading / manipulating images (hip package)
import Codec.Picture
import Codec.Picture.Extra

-- for sending serial data to the printer (serialport package)
import Data.Char
import Data.Bits
import Data.Word
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B

main = do
    eitherImg <- readImage "test.jpg"
    case eitherImg of
        Left msg -> print msg
        Right img -> do
            let printableImg = transformImage img
            savePngImage "out.png" (ImageY8 printableImg)
            sendToPrinter printableImg
            print "Done"




port = "/dev/ttyUSB0"
sendToPrinter :: Image Pixel8 -> IO ()
sendToPrinter img = do
    sp <- openSerial port defaultSerialSettings {commSpeed = CS19200}

    let stripesBytes = fmap (\stripe -> stripeToBytes stripe) (imageToStripes img) :: [[Char]]
    let stripesPrinterStrings = fmap (\stripeBytes -> stripeBytesToPrinterString stripeBytes ) stripesBytes :: [String]

    let totalBytesSent = foldl' (\printerStr -> send sp (B.pack printerStr)) 0 stripesPrinterStrings

    print ("Sent " ++ (show bytesSent) ++ " bytes to the printer.")



imageToStripes :: Image Pixel8 -> [Image Pixel8]
-- TODO

stripeToBytes :: Image Pixel8 -> [Char]
-- TODO


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
    | value < 100 = 0
    | otherwise = 255






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
