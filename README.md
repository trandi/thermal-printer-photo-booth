# thermal-printer-photo-booth
Take an old thermal printer like those that print receipts in a supermarket till, hack it for manual control, add a Raspberry Pi with its camera and obtain a minimalist and low-res photo booth...  With some Haskell code running the show, of course !

With a fresh checkout:

cabal sandbox init  
cabal install ...missing packages, like JuicyPixels, serialport etc. ...  
cabal build  
cabal run  
