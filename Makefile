run:
	cd src;	runhaskell Main.hs *.hs

fay:
	cd src; fay Fay.hs --html-wrapper
	cd src; open Fay.html
