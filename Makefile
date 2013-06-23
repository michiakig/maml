build:
	ml-build sources.cm Main.main

test:
	ml-build sources.cm Main.main > /dev/null
	sml @SMLload=sources.x86-darwin

clean:
	rm -f *.x86-darwin

