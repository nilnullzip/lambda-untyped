run:
	cd src;	runhaskell Main.hs *.hs

fay: .FORCE
	mkdir -p fay
	fay src/FayLC.hs --html-wrapper --strict AST,Parse,Reduce,FayLC,Help --include src --output fay/FayLC.js
	open fay/FayLC.html

clean:
	rm -r fay

.FORCE:
