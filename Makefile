DUNE=dune

all:
	$(DUNE) build @install

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

reinstall: uninstall install

doc:
	$(DUNE) build @doc

publish-doc: doc
	rm -rf .gh-pages
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -r _build/default/_doc/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f

publish: gh-pages
	opam-publish submit "./maildir.$(VERSION)"

.PHONY: all lib clean doc install uninstall test
