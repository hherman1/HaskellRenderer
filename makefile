run:	main.hs
	ghc main.hs
	(echo "read examples/parCube.dat" > .tempfile)
	cat .tempfile - | ./main; rm .tempfile
