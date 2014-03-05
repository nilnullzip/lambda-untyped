run:
	cd src;	runhaskell Main.hs *.hs

fay: .FORCE
	mkdir -p fay
	fay src/Fay.hs --html-wrapper --strict AST,Parse,Reduce --include src --output fay/Fay.js
	open fay/Fay.html

clean:
	rm -r fay

.FORCE:
