# R package `phtt`

This repository contains the R package `phtt` for estimating panel data models with time varying individual effects. 

The functions in this package are described in 

Bada, O., & Liebl, D. (2014). [phtt: Panel Data Analysis with Heterogeneous Time Trends in R](https://www.jstatsoft.org/index.php/jss/article/view/v059i06/768). Journal of Statistical Software, 59(6), 1–33. https://doi.org/10.18637/jss.v059.i06


Download the paper (PDF): [HERE](https://www.jstatsoft.org/index.php/jss/article/view/v059i06/768)


## Installation 

```{r}
library(devtools)
install_github("lidom/R-package-phtt/phtt")
```



### R Codes 


The following R codes reproduce the Figures of our article: 

Bada, O., & Liebl, D. (2014). [phtt: Panel Data Analysis with Heterogeneous Time Trends in R](https://www.jstatsoft.org/index.php/jss/article/view/v059i06/768). Journal of Statistical Software, 59(6), 1–33. https://doi.org/10.18637/jss.v059.i06


```{r}
## Load package
library("phtt")

## Load Data
data("Cigar", package = "phtt")
n <- 46
T <- 30
## Dependent variable: Cigarette-Sales per Capita
l.Consumption <- log(matrix(Cigar$sales, T, n))
## Independent variables: Consumer Price Index
cpi <- matrix(Cigar$cpi, T, n)
## Real Price per Pack of Cigarettes
l.Price <- log(matrix(Cigar$price, T, n)/cpi)
## Real Disposable Income per Capita
l.Income <- log(matrix(Cigar$ndi, T, n)/cpi)

## Figure 1:
scl <- 1.6
par(mfrow = c(1, 3), mar = c(6, 5, 5, 2.1))
matplot(l.Consumption, type = "l", main = "Log's of\nCigar-Consumption", ylab = "", 
  xlab = "Time", cex.main = scl, cex.lab = scl, cex.axis = scl)
matplot(l.Price, type = "l", main = "Log's of\nreal Prices", ylab = "", xlab = "Time", 
  cex.main = scl, cex.lab = scl, cex.axis = scl)
matplot(l.Income, type = "l", main = "Log's of\nreal Income", ylab = "", xlab = "Time", 
  cex.main = scl, cex.lab = scl, cex.axis = scl)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
graphics.off()

## Estimation of the KSS model
Cigar.KSS <- KSS(formula = l.Consumption ~ l.Price + l.Income)
(Cigar.KSS.summary <- summary(Cigar.KSS))

## Figure 2:
plot(Cigar.KSS.summary)
graphics.off()

## Estimation of the number of factors
OptDim(Obj = l.Consumption, criteria = "PC1")

(OptDim.obj <- OptDim(Obj = l.Consumption, criteria = c("PC3", "ER", "GR", "IPC1", 
  "IPC2", "IPC3"), standardize = TRUE))

## Figure 3:
plot(OptDim.obj)
graphics.off()

## This is for interactive use.
## KSS(formula = l.Consumption ~ -1 + l.Price + l.Income, consult.dim = TRUE)

## First differences of the data:
d.l.Consumption <- diff(l.Consumption)
d.l.Price <- diff(l.Price)
d.l.Income <- diff(l.Income)

## Estimation of the EUP model
(Cigar.Eup <- Eup(d.l.Consumption ~ -1 + d.l.Price + d.l.Income, dim.criterion = "PC3"))
summary(Cigar.Eup)

## Figure 4:
plot(summary(Cigar.Eup))
graphics.off()

## Estimation of the KSS model with additive individual effects
Cigar2.KSS <- KSS(formula = l.Consumption ~ l.Price + l.Income, additive.effects = "individual")
(Cigar2.KSS.summary <- summary(Cigar2.KSS))

## Figure 5:
scl <- 1.75
par(mar = c(6, 5, 5, 2.1))
plot(Cigar2.KSS.summary, cex.main = 1.25, cex.lab = scl, cex.axis = scl)
par(mar = c(5.1, 4.1, 4.1, 2.1))
graphics.off()

## Testing for model specification
twoways.obj <- Eup(d.l.Consumption ~ -1 + d.l.Price + d.l.Income, factor.dim = 0, 
  additive.effects = "twoways")
not.twoways.obj <- Eup(d.l.Consumption ~ -1 + d.l.Price + d.l.Income, factor.dim = 2, 
  additive.effects = "none")
try(checkSpecif(obj1 = twoways.obj, obj2 = not.twoways.obj, level = 0.01))

## 
Eup.obj <- Eup(d.l.Consumption ~ -1 + d.l.Price + d.l.Income, additive.effects = "twoways")
checkSpecif(Eup.obj, level = 0.01)

## 
KSS.obj <- KSS(l.Consumption ~ -1 + l.Price + l.Income, additive.effects = "twoways")
checkSpecif(KSS.obj, level = 0.01)

## Percentages of explained variances
coef(Cigar2.KSS)$Var.shares.of.loadings.param[1]
coef(Cigar2.KSS)$Var.shares.of.loadings.param[2]
coef(Cigar2.KSS)$Var.shares.of.loadings.param[3]
coef(Cigar2.KSS)$Var.shares.of.loadings.param[4]
coef(Cigar2.KSS)$Var.shares.of.loadings.param[5]
## 

## Figure 6:
scl <- 1
par(mfrow = c(1, 2))
matplot(coef(Cigar2.KSS)$Common.factors[, 1] %*% t(coef(Cigar2.KSS)$Ind.loadings.param[, 
  1]), type = "l", main = "Variance of time-varying indiv. effects\n in direction of the 1. common factor", 
  xlab = "Time", ylab = "", cex.main = scl, cex.lab = scl, cex.axis = scl)
matplot(coef(Cigar2.KSS)$Common.factors[, 2] %*% t(coef(Cigar2.KSS)$Ind.loadings.param[, 
  2]), type = "l", main = "Variance of time-varying indiv. effects\n in direction of the 2. common factor", 
  xlab = "Time", ylab = "", cex.main = scl, cex.lab = scl, cex.axis = scl)
par(mfrow = c(1, 1))
graphics.off()
```