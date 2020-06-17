my_packages <- c('knitr', 'nmw', 'lattice', 'tidyverse', 'kableExtra', 'nlme')
pacman::p_load(char = my_packages)
#knitr::write_bib(my_packages, file = "references/packages.bib")

knitr::opts_chunk$set(echo=TRUE, 
                      message = FALSE, 
                      warning = FALSE,
                      out.width="100%", 
                      dpi = 96, 
                      #fig.width = 1,
                      error = TRUE)

