.PHONY: all
all: html info

.PHONY: html
html: public

.PHONY: info
info: $(PKG).info

public: $(PKG).texi docs.css
	$(MAKEINFO) --html --css-include=docs.css $< -o $@
	touch public

%.info: %.texi
	$(MAKEINFO) $< -o $@

%.pdf: %.texi
	texi2pdf --clean $< >/dev/null

%.texi: %.org
	$(EMACS) --batch -Q $< --funcall org-texinfo-export-to-texinfo

.PHONY: clean
clean:
	rm -f *.texi

.PHONY: distclean
distclean: clean
	rm -rf public
	rm -f *.info
