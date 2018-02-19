cp index.R $1.R
zip -r releases/nmw-$1.zip $1.R README.Rmd data-raw/input.deck cover.jpg 03-EMAX 04-THEO bib.txt
rm $1.R

