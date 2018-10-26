manuscript.pdf: manuscript.Rmd template/plos-one.tmpl references.bib
%.pdf: %.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf *.pdf *.tex *_cache/ *_files/
