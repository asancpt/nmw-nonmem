pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

all:
	Rscript --quiet _render.R

readme:
	Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"
