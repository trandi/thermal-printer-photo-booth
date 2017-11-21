module PrinterTests where

import Printer
import Data.Bits
import Data.Char
import Data.Word
import Data.Foldable


prop_column8PixelsToPrinterChar :: [Word8] -> Bool
prop_column8PixelsToPrinterChar pixels = 
    column8PixelsToPrinterChar pixels == foldl' (\acc (i, bit) -> acc + bit * 2^i) 0 indexedBits
    where bits = fmap (\pixel -> if pixel == 0 then 1 else 0) pixels
          indexedBits = zip [0..] (reverse bits)
 

prop_stripeBytesToPrinterString :: [Char] -> Bool
prop_stripeBytesToPrinterString chars = 
    (length (stripeBytesToPrinterString chars)) 
    == (length sel_bit_img) + 2 + (length chars) + 1


prop_generateNlNh_size :: [Char] -> Bool
prop_generateNlNh_size chars = 
    (ord (head generated)) + (ord (head (tail generated))) * 2^8 == length chars
    where generated = generateNlNh chars


prop_generateNlNh_value :: [Char] -> Bool
prop_generateNlNh_value chars = True

