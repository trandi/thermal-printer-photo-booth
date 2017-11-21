import Test.QuickCheck
import Test.HUnit.Text

import PrinterTests
import ImageProcessingTests



main :: IO ()
main = do
    runTestTT imageProcessingTests
    quickCheck prop_imageToStripes

    quickCheck prop_generateNlNh_size
    quickCheck prop_generateNlNh_value
    quickCheck prop_stripeBytesToPrinterString
    quickCheck prop_column8PixelsToPrinterChar

