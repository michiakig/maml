test:
	ml-build sources.cm TypeInfTests.main > /dev/null
	sml @SMLload=sources.x86-darwin

clean:
	rm -f *.x86-darwin

