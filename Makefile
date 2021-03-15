SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard PensionAge_*.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd PensionAge.Rmd
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"

default: $(TARGETS)

clean:
	latexmk -c
	rm PensionAge*.tex
	rm PensionAge*.pdf
