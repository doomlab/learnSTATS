## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="25%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/ndis.jpg")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(datasets)
data("quakes")
str(quakes)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
table(quakes$mag)

## ----echo=TRUE, fig.height=3, fig.width=6, message=FALSE, warning=FALSE----
hist(quakes$mag, breaks = 24)

## ---- echo = FALSE, out.width="60%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/Skew_PosvsNeg.png")

## ---- echo = FALSE, out.width="60%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/Kurtosis.png")

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
hist(quakes$mag)

## ----message=FALSE--------------------------------------
library(moments)
skewness(quakes$mag)

library(psych)
describe(quakes$mag)

## -------------------------------------------------------
#moments
kurtosis(quakes$mag)

#psych
describe(quakes$mag)

## -------------------------------------------------------
summary(quakes$mag)

## ----message = FALSE------------------------------------
library(pastecs)
stat.desc(quakes)

## ----message = FALSE------------------------------------
library(Hmisc)
Hmisc::describe(quakes)

## ----message = FALSE------------------------------------
library(psych)
psych::describe(quakes)

## -------------------------------------------------------
mean(quakes$mag)

## -------------------------------------------------------
median(quakes$mag)

## ----message=FALSE--------------------------------------
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(quakes$mag)

## ---- echo = FALSE, out.width="40%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/bimodal_distribution.png")

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/Capture_01.png")

## -------------------------------------------------------
range(quakes$mag)
psych::describe(quakes$mag)

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/introDA2/interquartile_range.png")

## -------------------------------------------------------
quantile(quakes$mag)
summary(quakes$mag)

## -------------------------------------------------------
quantile(quakes$mag, c(0.05,0.50,0.75,0.95))

## ----message=FALSE--------------------------------------
var(quakes$mag)

## -------------------------------------------------------
sd(quakes$mag)

## ----message = FALSE------------------------------------
quakes$zscore <- scale(quakes$mag)
head(quakes$zscore)
str(quakes$zscore)
mean(quakes$mag)
sd(quakes$mag)

## -------------------------------------------------------
cov(quakes$mag, quakes$depth)

## ----message=FALSE, warning=FALSE-----------------------
cor(quakes$mag, quakes$depth)

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
library(corrplot)
corrplot(cor(quakes), order = "hclust")

