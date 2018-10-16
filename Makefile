prereqs := team-structures.R template/plos-one.tmpl references.bib
team-structures.pdf: manuscript.Rmd $(prereqs)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
