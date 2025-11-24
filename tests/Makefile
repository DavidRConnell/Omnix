TEST_EXTS := html tex txt md

# If TEST_FILES passed in top level call, use those otherwise use the wildcard
# to populate TEST_FILES. Effectively TEST_FILES ?=, but the top level export
# defines TEST_FILES even if it is empty so the ?= always sees it as defined
# and doesn't set the default.
ifeq ($(strip $(TEST_FILES)),)
	TEST_FILES := $(wildcard test_*.org)
endif

ifeq ($(strip $(REFERENCE_FILES)),)
	REFERENCE_FILES := $(wildcard reference_*.org)
endif

TEST_OUTPUTS := $(foreach ext,$(TEST_EXTS),$(TEST_FILES:.org=.$(ext)))
REFERENCE_OUTPUTS := $(foreach ext,$(TEST_EXTS),$(REFERENCE_FILES:.org=.$(ext)))
REFERENCE_OUTPUTS += $(REFERENCE_FILES:.org=.pdf)

.PHONY: all
all: check

.PHONY: check
check: $(TEST_OUTPUTS)

.PHONY: references
references: $(REFERENCE_OUTPUTS)

define EXPORTER
%.$(1): %.org $(SRC_FILES)
	@echo Building: "$$@"
	$$(CONVERT) --eval '(batch-ox-export "$$@" "$$<")'
	@echo
endef

%.tex: CONVERT += -l init-pdf-ox.el
%.md: CONVERT += --eval "(require 'ox-md)"
$(foreach ext,$(TEST_EXTS),$(eval $(call EXPORTER,$(ext))))

FIG_NAME := images/lines
test_sansext.html: $(FIG_NAME).svg
test_sansext.tex: $(FIG_NAME).pdf
test_sansext.txt: $(FIG_NAME).png
test_sansext.md: $(FIG_NAME).svg

images/%: fixtures/make_figs.py
	python fixtures/make_figs.py "$@"

.PHONY: pdfs
pdfs: $(TEST_FILES:.org=.pdf)

%.pdf: %.tex
	@$(shell [[ -d build ]] || build)
	latexmk -$(LATEX) -g -outdir="build" $<
	mv build/*.pdf "$@"

.PHONY: clean
clean:
	rm -rf build
	rm -f $(FIG_NAME).*

.PHONY: clean-dist
clean-dist: clean
	rm -f $(TEST_OUTPUTS)
	rm -f $(REFERENCE_OUTPUTS)
	rm -f $(TEST_FILES:.org=.pdf)
	rm -f $(REFERENCE_FILES:.org=.pdf)
