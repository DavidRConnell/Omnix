export TOP_DIR := $(PWD)
export SRC_FILES := $(wildcard $(TOP_DIR)/*.el)
export TEST_FILES

EMACS ?= emacs
INIT_EL ?= "(progn (require 'omnix) (require 'batch-ox))"
LOAD_PATH ?= -L $(TOP_DIR) -L $(TOP_DIR)/vendor/batch-ox
export CONVERT := $(EMACS) -Q --batch $(LOAD_PATH) --eval $(INIT_EL)


.PHONY: check
check:
	$(MAKE) -C tests check

.PHONY: check-pdfs
check-pdfs:
	$(MAKE) -C tests pdfs

.PHONY: clean
clean:
	$(MAKE) -C tests clean
