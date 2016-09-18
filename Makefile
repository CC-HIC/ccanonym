all:
	Rscript install.r man
check:
	R CMD check '.'
test:
	@Rscript -e 'library(devtools); test()'

report:
	@Rscript -e "library(knitr); library(ccreport); knit('sdc.rmd')"
	@Rscript -e "library(rmarkdown); render('sdc.md')"


clean:
	rm -rf src/*.o src/*.so
	rm -rf man
