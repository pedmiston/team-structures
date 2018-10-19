slides.pdf: slides.Rmd team-structures.R template/slides.tmpl references.bib
team-structures.pdf: manuscript.Rmd team-structures.R template/plos-one.tmpl references.bib
%.pdf: %.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
