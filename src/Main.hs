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

import Text.Printf

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

    printerConfig_RemoveSpacingBetweenLines port
    
    let stripes = imageToStripes img :: [Image Pixel8]

    --saveImages stripes
    
    let stripesBytes = fmap (\stripe -> stripeToBytes stripe) (reverse stripes) :: [[Char]]
    let stripesPrinterStrings = fmap (\stripeBytes -> stripeBytesToPrinterString stripeBytes ) stripesBytes :: [String]
    
    --printStringsAsHex stripesPrinterStrings

    -- below will actuall write to the serial port. 
    let totalBytesSentIO = foldl' (sendStringToPrinter port) (return 0) stripesPrinterStrings

    totalBytesSent <- totalBytesSentIO
    print ("Sent " ++ (show totalBytesSent) ++ " bytes to the printer.")

    printerConfig_ReinitPrinterEmptylinesCut port






sendStringToPrinter :: SerialPort -> IO Int -> String -> IO Int 
sendStringToPrinter port bytesSentUntilNowIO printerStr = do
    bytesSent <- send port (B.pack printerStr)
    -- below forces us to wait until the printer has processed the above data.
    -- slows things down but avoids errors if sending more than 4KB whichis the buffer of the printer
    receivedBytes <- recv port 1000
    bytesSentUntilNow <- bytesSentUntilNowIO
    return (bytesSentUntilNow + bytesSent)
    

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
    foldl' (\acc (index, pixelValue) -> acc + if pixelValue == 0 then 2^index else 0 ) 0 pixelsWithIndices
    where
        pixelsWithIndices = zip [0..] (reverse pixels) :: [(Int, Pixel8)]
        


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



sel_bit_img = [chr 0x1B, chr 0x2A, chr 0x00]  -- ESC*0
new_line = chr 0x0A
-- each element represents 8 vertical bits where black dots are 1 and white are 0
-- basically it's 1 column in the 8 bits wide horisontal stripe
stripeBytesToPrinterString :: [Char] -> String
stripeBytesToPrinterString dataBytes = 
    sel_bit_img ++ (generateNlNh dataBytes) ++ dataBytes ++ [new_line]

-- this is simply the size of the provided array, written on 2 bytes, the Low and High (in this order)
generateNlNh :: [Char] -> [Char]
generateNlNh list = [chr (l .&. 0xFF), chr ((shift l (-8)) .&. 0xFF)]
    where l = length list


printerConfig_RemoveSpacingBetweenLines :: SerialPort -> IO ()
printerConfig_RemoveSpacingBetweenLines port = do
    -- set no spacing between lines  "ESC 3 n" (where n is 0)
    sendStringToPrinter port (return 0) [chr 0x1B, chr 0x33, chr 0, new_line]
    return ()

printerConfig_ReinitPrinterEmptylinesCut :: SerialPort -> IO ()
printerConfig_ReinitPrinterEmptylinesCut port = do
    -- ESC @ for reinit the printer, then new lines, then ESC i   for cutting
    sendStringToPrinter port (return 0) [chr 0x1B, chr 0x40, new_line, new_line, new_line, new_line, chr 0x1B, chr 0x69, new_line]
    return ()



-- *********** DEBUG utility functions ***************

printStringsAsHex :: [String] -> IO ()
printStringsAsHex strs = do
    sequence (fmap (\(i, str) -> do { print (show i) ; charsAsHex str ; print ""} ) indexedStrings)
    print ("Done printing strings as HEX.")
    where
        indexedStrings = zip [0..] strs

-- takes each char in this string and displays it's hex value
charsAsHex :: String -> IO [()]
charsAsHex str = sequence (fmap (\c -> printf "%02x" c) str)

saveImages :: [Image Pixel8] -> IO ()
saveImages imgs = do
    temp <- sequence (fmap (\(i, stripe) -> savePngImage ("out"++ (show i) ++ ".png") (ImageY8 stripe)) indexedImgs )
    print ("Images saved " ++ show (length temp))
    where
        indexedImgs = zip [0..] imgs


