rpkg_name:=ccanonym

all:
	Rscript -e 'library(methods); library(roxygen2); roxygenize(".")'
	R CMD INSTALL .
#	Rscript install.r man

check:
	R CMD check '.'

test:
	@Rscript -e 'library(devtools); test()'

manual:
	R CMD Rd2pdf . --force

report:
	@Rscript -e "library(knitr); knit('vignettes/demo.rmd')"
	@Rscript -e "library(rmarkdown); render('demo.md')"

clean:
	rm -rf src/*.o src/*.so
	rm -rf man
	rm ..pdf

idhs:
	rsync -av . /tmp/${rpkg_name} --exclude '.*' --exclude '*.so' --exclude '*.o'
	zip -r ../${rpkg_name}.zip /tmp/${rpkg_name}
	rm -r /tmp/${rpkg_name}
