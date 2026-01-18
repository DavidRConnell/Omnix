export PKG := omnix
export TOP_DIR := $(PWD)
export SRC_DIR := $(TOP_DIR)/lisp
SRC_FILES := $(wildcard $(SRC_DIR)/*.el)
export BYTE_FILES := $(filter-out $(SRC_DIR)/omnix-pkg.elc,$(SRC_FILES:.el=.elc))
export LATEX ?= lualatex

export TEST_FILES
export REFERENCE_FILES
export TEST_EXTS

export EMACS ?= emacs
INIT_EL ?= "(progn (require 'omnix) (require 'batch-ox) (omnix-global))"
LOAD_PATH ?= -L $(SRC_DIR) -L $(TOP_DIR)/vendor/batch-ox
export EMACS_CMD := $(EMACS) -Q --batch $(LOAD_PATH) --eval $(INIT_EL)

VERSION ?= $(shell git describe --tags 2>/dev/null | cut -c2-)
ifeq ($(VERSION),)
	VERSION = 0.1.0-alpha
endif

PACKAGE_NAME := $(PKG)-$(VERSION)

.PHONY: all
all: build

.PHONY: build
build: $(BYTE_FILES)

%.elc: %.el
	@echo Compiling: $(notdir $<)
	$(EMACS_CMD) --eval '(byte-compile-file "$<")'
	@echo

.PHONY: dist
dist: check-package $(PACKAGE_NAME).tar docs

$(PACKAGE_NAME).tar: $(SRC_FILES)
	sed -i 's/\(Version:\).*/\1 $(VERSION)/g' $(SRC_DIR)/omnix.el
	sed -i 's/[0-9]\.[0-9]\.[0-9][^"]*/$(VERSION)/g' $(SRC_DIR)/omnix-pkg.el
	tar --transform='s,^,$(PACKAGE_NAME)/,' -c -f "$@" -C $(SRC_DIR) $(notdir $(SRC_FILES))

.PHONY: docs
docs:
	$(MAKE) -C docs all

# Regular org-export based tests to confirm Omnix is behaving as expected.
.PHONY: check
check: build
	$(MAKE) -C tests check

# Lint package and collect byte-compiler warnings.
.PHONY: check-package
check-package: clean-bytefiles
	$(EMACS_CMD) \
	  --eval "(require 'package-lint)" \
	  --eval '(setq package-lint-main-file "$(SRC_DIR)/omnix.el"))' \
	  -f "package-lint-batch-and-exit" $(SRC_FILES) || true

	$(MAKE) build

# Check "reference" tests used to see normal ox behavior (i.e. without Omnix).
.PHONY: check-references
references: build
	$(MAKE) -C tests references

# Pass check target's generated tex files through LaTeX to produce PDFs.
# Often the tex files are enough but it's useful to see the final product
# occasionally.
.PHONY: check-pdfs
check-pdfs: build
	$(MAKE) -C tests pdfs

.PHONY: clean
clean:
	$(MAKE) -C tests clean
	rm -f $(BYTE_FILES)
	rm -f *.tar
	$(MAKE) -C docs clean

.PHONY: distclean
distclean: clean
	$(MAKE) -C tests distclean
	$(MAKE) -C docs distclean
