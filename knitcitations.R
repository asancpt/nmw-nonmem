## ------------------------------------------------------------------------
library(knitcitations)
cleanbib()
options("citation_format" = "pandoc")
citep("dx.doi.org/10.12793/tcp.2015.23.1.1")
citep("dx.doi.org/10.12793/tcp.2016.24.4.161")
citet(citation(package = "nmw", lib.loc = "./lib"))
write.bibtex(file = "bibliography.bib")

## ----results = 'asis'----------------------------------------------------
bibliography()

