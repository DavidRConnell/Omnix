export TOP_DIR := $(PWD)
export SRC_DIR := $(TOP_DIR)/lisp
export SRC_FILES := $(wildcard $(SRC_DIR)/*.el)
export LATEX ?= lualatex

export TEST_FILES
export REFERENCE_FILES
export TEST_EXTS

EMACS ?= emacs
INIT_EL ?= "(progn (require 'omnix) (require 'batch-ox) (omnix-global))"
LOAD_PATH ?= -L $(SRC_DIR) -L $(TOP_DIR)/vendor/batch-ox
export CONVERT := $(EMACS) -Q --batch $(LOAD_PATH) --eval $(INIT_EL)

VERSION ?= $(shell git describe --tags 2>/dev/null | cut -c2-)
ifeq ($(VERSION),)
	VERSION = 0.1.0-alpha
endif

PACKAGE_NAME := omnix-$(VERSION)

.PHONY: all
all: dist

.PHONY: dist
dist: $(PACKAGE_NAME).tar

$(PACKAGE_NAME).tar: $(SRC_FILES)
	sed -i 's/\(Version:\).*/\1 $(VERSION)/g' $(SRC_DIR)/omnix.el
	sed -i 's/[0-9]\.[0-9]\.[0-9][^"]*/$(VERSION)/g' $(SRC_DIR)/omnix-pkg.el
	tar --transform='s,^,$(PACKAGE_NAME)/,' -c -f "$@" -C $(SRC_DIR) $(notdir $(SRC_FILES))

.PHONY: check
check:
	$(MAKE) -C tests check

.PHONY: references
references:
	$(MAKE) -C tests references

.PHONY: check-pdfs
check-pdfs:
	$(MAKE) -C tests pdfs

.PHONY: clean
clean:
	$(MAKE) -C tests clean
	rm -f *.tar

.PHONY: distclean
distclean: clean
	$(MAKE) -C tests distclean
