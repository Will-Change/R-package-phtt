## ################################
##
## Make Package Codes
##
## ################################

## Remove pkg 
remove.packages("phtt")

## Create/update documentation and (re-)write NAMESPACE
devtools::document("phtt")

## CRAN-check pkg
devtools::check("phtt")       # check the package

## Install
devtools::install_local("phtt", force = TRUE)

devtools::install_github("lidom/R-package-phtt/phtt", force = TRUE)
##
## #################################





data(Cigar)
## Panel-Dimensions:
N <- 46
T <- 30
## Dependent variable:
## Cigarette-Sales per Capita
l.Consumption    <- log(matrix(Cigar$sales, T,N))
## Independent variables:
## Consumer Price Index
cpi        <- matrix(Cigar$cpi, T,N)
## Real Price per Pack of Cigarettes 
l.Price  <- log(matrix(Cigar$price, T,N)/cpi)
## Real Disposable Income per Capita  
l.Income    <- log(matrix(Cigar$ndi,   T,N)/cpi)

## Estimation:
KSS.fit      <- KSS(l.Consumption~l.Price+l.Income, CV=TRUE)
