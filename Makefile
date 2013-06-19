test:
	ml-build typeinf.cm TypeInfTests.main > /dev/null
	sml @SMLload=typeinf.x86-darwin
