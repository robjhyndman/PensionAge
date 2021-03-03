all: wp anzjs

wp:
	Rscript -e 'rmarkdown::render("PensionAge_wp.Rmd")'

anzjs:
	Rscript -e 'rmarkdown::render("PensionAge_anzjs.Rmd")'

clean:
	latexmk -c
	rm PensionAge*.tex
	rm PensionAge*.pdf
