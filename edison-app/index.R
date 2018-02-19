#!/SYSTEM/R/3.3.3/bin/Rscript

# 1. setup ----

# library folders
is.linux <- grepl('linux', R.version$os)
if (is.linux) .libPaths(c("./lib", '/SYSTEM/R/3.3.3/lib64/R/library'))
mylib <- c("nmw", "lattice", "compiler", "knitr", "markdown")
lapply(mylib, library, character.only = TRUE) 

# current status
print(lapply(.libPaths(), dir))
print(capabilities())
print(sessionInfo())

# create result folder
if (length(intersect(dir(), 'result')) == 0) system('mkdir result')

# arguments
Args <- commandArgs(trailingOnly = TRUE) 
if (identical(Args, character(0))) Args <- c("-inp", "data-raw/input.deck")
if (Args[1] == "-inp") InputParameter <- Args[2] # InputPara.inp

# 2. main ----

# nTheta x 2
# nEta x 1
# nEps x 1
# THETAinit x~x 10 100
# OMinit x 0.2
# SGinit x 1

inputFirst <- read.table(InputParameter, row.names = 1, sep = "=", 
                         comment.char = ";", strip.white = TRUE, as.is = TRUE)
write.csv(inputFirst, "result/inputFirst.csv", row.names = TRUE)

# 3. report ----

system("cp cover.jpg result")
knit(input = "README.Rmd", output = "README.md")
markdownToHTML('README.md', "result/README.html", 
                         options = c("toc", "mathjax"))
print("Complete.")

# old ----

#inputRaw <- t(input)
#rownames(inputRaw) <- c("Body Weight", 
#                        "Caffeine Dose", "Simulation Subject N", "Log Y-axis", "Plot Format", 
#                        "Multiple Dosing Interval", "Multiple Dosing")
#
#inputSummary <- inputRaw %>% data.frame() %>% 
#    rownames_to_column(var = "Input") %>% 
#    select(1, Value = 2) %>% 
#    mutate(Unit = c("kg", "mg", "", "", "", "hour", "times"))
#
#write.csv(inputSummary, "result/Data_Input.csv", row.names = FALSE)
