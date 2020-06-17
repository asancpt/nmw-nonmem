all: gitbook pdf epub

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

pdf:
	Rscript --quiet _render.R "bookdown::pdf_book"

epub:
	Rscript --quiet _render.R "bookdown::epub_book"

readme:
	Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"

clean:
	rm -rf docs _bookdown_files


