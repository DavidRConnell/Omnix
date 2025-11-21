TEST_EXTS := html tex txt
TEST_INPUTS := $(wildcard test_*.org)
TEST_OUTPUTS := $(foreach ext,$(TEST_EXTS),$(TEST_INPUTS:.org=.$(ext)))

.PHONY: all
all: check

.PHONY: check
check: $(TEST_OUTPUTS)

define EXPORTER
%.$(1): %.org $(SRC_FILES)
	@echo Building "$$@" from "$$^"
	$(CONVERT) --eval '(batch-ox-export "$$@" "$$<")'
endef

$(foreach ext,$(TEST_EXTS),$(eval $(call EXPORTER,$(ext))))

.PHONY: pdfs
pdfs: $(TEST_INPUTS:.org,.pdf)

%.pdf: %.tex
	@[ -d build ] || build
	latexmk -$(LATEX) -g -outdir="build" $<

.PHONY: clean
clean:
	rm -f $(TEST_OUTPUTS)
	rm -rf build
