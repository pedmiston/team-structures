prereqs := team-structures.R template/plos-one.tmpl references.bib
team-structures.pdf: team-structures.Rmd $(prereqs)
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
