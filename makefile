run:	main.hs
	ghc --make -O2 main.hs
	(echo "read examples/parCube.dat" > .tempfile)
	cat .tempfile - | ./main; rm .tempfile
