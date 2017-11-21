-- pure functions generating the right commands for the printer.
-- Sending the actual commands to the printer is done in Main.hs given that they are impure
module Printer where

import Codec.Picture
import Data.Char
import Data.Foldable
import Data.Bits



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


-- set no spacing between lines  "ESC 3 n" (where n is 0)
printerConfig_RemoveSpacingBetweenLines :: String
printerConfig_RemoveSpacingBetweenLines = [chr 0x1B, chr 0x33, chr 0, new_line]

-- ESC @ for reinit the printer, then new lines, then ESC i   for cutting
printerConfig_ReinitPrinterEmptylinesCut :: String
printerConfig_ReinitPrinterEmptylinesCut = [chr 0x1B, chr 0x40, new_line, new_line, new_line, new_line, chr 0x1B, chr 0x69, new_line]

