test:
	ml-build sources.cm TypeInfTests.main > /dev/null
	sml @SMLload=sources.x86-darwin

