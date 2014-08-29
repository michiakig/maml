default:
	ml-build sources.cm Main.main

test:	default
	sml @SMLload=sources.x86-linux

clean:
	rm -f *.x86-linux
	rm -rf .cm/ src/.cm/ tests/.cm/
