---
title: NONMEM workshop 2017
author: Sungpil Han, Kyun-Seop Bae
date: 2017-02-22
bibliography: bibliography.bib
output:
  html_document:
    toc: yes
    keep_md: yes
  word_document:
    toc: yes
  pdf_document:
    includes:
      in_header: preamble.tex
editor_options: 
  chunk_output_type: console
---







![Cover](cover.jpg)



<https://github.com/asancpt/nmw2017edison>
license: GPL-3

## Introduction

NONMEM Workshop 2017<ec>óê<ec>Ñú <ec>Ç¨<ec>ö©<eb>êú nmw <ed>å®<ed>Ç§ÏßÄÎ•<bc> <ec>Ç¨<ec>ö©<ed>ïú Edison <ec>Ç¨<ec>ù¥<ec>ñ∏<ec>ä§ <ec>ï±<ec>ûÖ<eb>ãà<eb>ã§. [@Kim_2015;@Bae_2016;@Bae_2017]

## Result

A table (head) and a figure of input dataset is shown below.

### Initial values


```r
kable(inputFirst, format = "markdown")
```



|          |V2                                          |
|:---------|:-------------------------------------------|
|Dataset   |Theoph                                      |
|Method    |ZERO                                        |
|nTheta    |3                                           |
|nEta      |3                                           |
|nEps      |2                                           |
|THETAinit |2, 50, 0.1                                  |
|OMinit    |0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2 |
|SGinit    |0.1, 0, 0, 0.1                              |

### Input Table




```r
kable(head(DataAll, n = 20), caption = "input data", format = "markdown")
```



|ID |  TIME|    DV|
|:--|-----:|-----:|
|1  |  0.00|  0.74|
|1  |  0.25|  2.84|
|1  |  0.57|  6.57|
|1  |  1.12| 10.50|
|1  |  2.02|  9.66|
|1  |  3.82|  8.58|
|1  |  5.10|  8.36|
|1  |  7.03|  7.47|
|1  |  9.05|  6.89|
|1  | 12.12|  5.94|
|1  | 24.37|  3.28|
|2  |  0.00|  0.00|
|2  |  0.27|  1.72|
|2  |  0.52|  7.91|
|2  |  1.00|  8.31|
|2  |  1.92|  8.33|
|2  |  3.50|  6.85|
|2  |  5.02|  6.08|
|2  |  7.03|  5.40|
|2  |  9.00|  4.55|

### Figure



![Concentration-time curve of Theoph](xyplot.jpg)

### Method Calculation

- Dataset: Theoph
- Method: ZERO


```r
########
PREDFILE <- ifelse(NMDataset == "Emax", "03-Emax/PRED.R", "04-THEO/PRED.R")

InitPara = InitStep(DataAll, THETAinit=THETAinit, OMinit=OMinit, SGinit=SGinit, 
                    nTheta=nTheta, LB=LB, UB=UB, METHOD=METHOD, PredFile=PREDFILE)
```

```
## Error in InitStep(DataAll, THETAinit = THETAinit, OMinit = OMinit, SGinit = SGinit, : unused arguments (nTheta = nTheta, PredFile = PREDFILE)
```

```r
(EstRes = EstStep())           # 0.6200359 secs, 0.4930282 secs
```

```
## Error in rep(0.1, e$nPara): invalid 'times' argument
```

```r
(CovRes = CovStep())
```

```
## Error in 1:e$nTheta: argument of length 0
```

```r
PostHocEta() # FinalPara from EstStep()
```

```
## Error in 1:e$nTheta: argument of length 0
```

```r
get("EBE", envir=e)
```

```
## Error in get("EBE", envir = e): object 'EBE' not found
```

## Appendix

### Examples for Initial Values

#### Emax
```
nTheta = 2
nEta = 1
nEps = 1

THETAinit = 10, 100
OMinit = 0.2
SGinit = 1
```

#### Theoph (ZERO, CONC)
```
nTheta = 3
nEta = 3
nEps = 2

THETAinit = 2, 50, 0.1
OMinit = 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2
SGinit = 0.1, 0, 0, 0.1
```

#### Theoph (LAPL)

```
THETAinit = 4, 50, 0.2
```

The other values are the same with those of `Theoph (ZERO, CONC)`.

## Reference

[1] K. Bae. _nmw: NONMEM Workshop 2017_. R package version 0.1.0. 2017. <URL:
http://optimizer.r-forge.r-project.org/>.

[2] K. Bae and D. Yim. <U+FFFD>úR-based reproduction of the estimation process hidden behind
NONMEM¬Æ Part 2: First-order conditional estimation<U+FFFD><U+FFFD>. In: _Translational and Clinical
Pharmacology_ 24.4 (2016), p. 161. DOI: 10.12793/tcp.2016.24.4.161. <URL:
https://doi.org/10.12793%2Ftcp.2016.24.4.161}.>

[3] M. Kim, D. Yim and K. Bae. <U+FFFD>úR-based reproduction of the estimation process hidden
behind NONMEM¬Æ Part 1: first-order approximation method<U+FFFD><U+FFFD>. In: _Translational and Clinical
Pharmacology_ 23.1 (2015), p. 1. DOI: 10.12793/tcp.2015.23.1.1. <URL:
https://doi.org/10.12793%2Ftcp.2015.23.1.1}.>

## Bibliography

