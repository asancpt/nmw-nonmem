rm nmw.zip
cp index.R $1.R
zip -r releases/nmw-$1.zip $1.R README.Rmd data-raw input.deck cover.jpg
