.POSIX:
SHELL = /bin/sh
.SUFFIXES:

PKG = cabin-mixer
COLLECT = cabin-mixer

EXES = CabinMixer.app/Contents/MacOS/CabinMixer \
       CabinMixer \
       CabinMixer.exe

RACO = raco

install:
	$(RACO) pkg install --name $(PKG) --auto $(RACO_INSTALL_ARGS)

uninstall:
	$(RACO) pkg remove $(PKG)

setup:
	$(RACO) setup $(RACO_SETUP_ARGS) --pkgs $(PKG)

test:
	$(RACO) test $(RACO_TEST_ARGS) --package $(PKG)

check-deps:
	$(RACO) setup $(RACO_SETUP_ARGS) --check-pkg-deps --pkgs $(PKG)

fix-deps:
	$(RACO) setup $(RACO_SETUP_ARGS) --fix-pkg-deps --pkgs $(PKG)

fix-doc-index:
	$(RACO) setup $(RACO_SETUP_ARGS) --doc-index --pkgs $(PKG)

docs/cabin-mixer/index.html:
	scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --htmls --dest ./docs ./scribblings/cabin-mixer.scrbl

$(EXES): gui/mixer.rkt
	$(RACO) exe --gui -o CabinMixer gui/mixer.rkt

# POSIX leaves $< unspecified in these target rules
macOS-CabinMixer: CabinMixer.app/Contents/MacOS/CabinMixer
	$(RACO) distribute $@ CabinMixer.app
linux-CabinMixer: CabinMixer
	$(RACO) distribute $@ CabinMixer
windows-CabinMixer: CabinMixer.exe
	$(RACO) distribute $@ CabinMixer.exe

linux-CabinMixer.tar.gz: linux-CabinMixer
	tar cvf - linux-CabinMixer | gzip >$@
macOS-CabinMixer.tar.gz: macOS-CabinMixer
	tar cvf - macOS-CabinMixer | gzip >$@
windows-CabinMixer.zip: windows-CabinMixer
	7z a $@ windows-CabinMixer
