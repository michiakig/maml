test:
	ml-build typeinf.cm TypeInfTests.main
	sml @SMLload=typeinf.x86-darwin
