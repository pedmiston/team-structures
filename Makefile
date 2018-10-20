slides.pdf: slides.Rmd team-structures.R template/slides.tmpl references.bib
team-structures.pdf: manuscript.Rmd team-structures.R template/plos-one.tmpl references.bib
notes.pdf: slides.Rmd
	sed -e 's/^<!--//' \
		-e 's/^-->//' \
		-e 's/\begincols//' \
		-e 's/\begincol{0.48\\textwidth}//' \
		-e 's/\endcols//' \
		-e 's/\endcol//' \
		$< > notes.Rmd
	Rscript -e 'rmarkdown::render("notes.Rmd", output_format = "pdf_document", output_file = "$@")'
%.pdf: %.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf *_cache/ *_files/ slides.pdf
