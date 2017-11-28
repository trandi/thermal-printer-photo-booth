-- Main logic and everything to do with IO
module Main where

import ImageProcessing
import Printer

import Codec.Picture
import Data.Char
import Data.Foldable
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import Text.Printf
import System.Environment


port = "/dev/ttyUSB0"


main = do
    progArgs <- getArgs
    eitherImg <- readImage (progArgs !! 0)
    let scaleImg = (progArgs !! 1) == "scale"
    case eitherImg of
        Left msg -> print msg
        Right img -> do
            let printableImg = transformImage scaleImg img
            savePngImage "transformed.png" (ImageY8 printableImg)
            sendImgToPrinter printableImg
            print "Done"




sendImgToPrinter :: Image Pixel8 -> IO ()
sendImgToPrinter img = do
    port <- openSerial port defaultSerialSettings {commSpeed = CS19200}

    sendStringToPrinter port printerConfig_RemoveSpacingBetweenLines
    
    let stripes = imageToStripes img :: [Image Pixel8]

    --saveImages stripes
    
    let stripesBytes = fmap (\stripe -> stripeToBytes stripe) (reverse stripes) :: [[Char]]
    let stripesPrinterStrings = fmap (\stripeBytes -> stripeBytesToPrinterString stripeBytes ) stripesBytes :: [String]
    
    --printStringsAsHex stripesPrinterStrings

    -- below will actuall write to the serial port. 
    let totalBytesSentIO = foldl' (sendStringToPrinterCountBytes port) (return 0) stripesPrinterStrings

    totalBytesSent <- totalBytesSentIO
    print ("Sent " ++ (show totalBytesSent) ++ " bytes to the printer.")

    sendStringToPrinter port printerConfig_ReinitPrinterEmptylinesCut




sendStringToPrinter :: SerialPort -> String -> IO ()
sendStringToPrinter port str = do
    sendStringToPrinterCountBytes port (return 0) str
    return ()

sendStringToPrinterCountBytes :: SerialPort -> IO Int -> String -> IO Int 
sendStringToPrinterCountBytes port bytesSentUntilNowIO printerStr = do
    bytesSent <- send port (B.pack printerStr)
    -- below forces us to wait until the printer has processed the above data.
    -- slows things down but avoids errors if sending more than 4KB whichis the buffer of the printer
    receivedBytes <- recv port 1000
    bytesSentUntilNow <- bytesSentUntilNowIO
    return (bytesSentUntilNow + bytesSent)
    




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


