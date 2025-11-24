TEST_EXTS := html tex txt

# If TEST_FILES passed in top level call, use those otherwise use the wildcard
# to populate TEST_FILES. Effectively TEST_FILES ?=, but the top level export
# defines TEST_FILES even if it is empty so the ?= always sees it as defined
# and doesn't set the default.
ifeq ($(strip $(TEST_FILES)),)
	TEST_FILES := $(wildcard test_*.org)
endif

TEST_OUTPUTS := $(foreach ext,$(TEST_EXTS),$(TEST_FILES:.org=.$(ext)))

.PHONY: all
all: check

.PHONY: check
check: $(TEST_OUTPUTS)

define EXPORTER
%.$(1): %.org $(SRC_FILES)
	@echo Building: "$$@"
	$$(CONVERT) --eval '(batch-ox-export "$$@" "$$<")'
	@echo
endef

%.tex: CONVERT += -l init-pdf-ox.el
%.md: CONVERT += --eval "(require 'ox-md)"
$(foreach ext,$(TEST_EXTS),$(eval $(call EXPORTER,$(ext))))

.PHONY: pdfs
pdfs: $(TEST_FILES:.org=.pdf)

%.pdf: %.tex
	@$(shell [[ -d build ]] || build)
	latexmk -$(LATEX) -g -outdir="build" $<
	mv build/*.pdf "$@"

.PHONY: clean
clean:
	rm -f $(TEST_OUTPUTS)
	rm -f $(TEST_FILES:.org=.pdf)
	rm -rf build
