my_packages <- c('knitr', 'nmw', 'lattice', 'tidyverse', 'kableExtra')
pacman::p_load(char = my_packages)

knitr::opts_chunk$set(message = FALSE, warning = FALSE, error = TRUE)

knitr::write_bib(my_packages, file = "references/packages.bib")

