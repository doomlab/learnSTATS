## ---- include = FALSE----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------
options(scipen = 20)
#dataset, rows/columns, function 
round(apply(quakes, 2, mean), 2)

## ------------------------------------------------
lapply(quakes, mean)

## ------------------------------------------------
round(sapply(quakes, mean),2)

## ------------------------------------------------
tapply(quakes$mag, #dependent variable
       list(quakes$stations), #independent variable(s)
       mean) #function

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/introDA3/standard_error.png")

## ------------------------------------------------
psych::describe(quakes$mag)

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/introDA3/confidence_intervals.png")

## ----echo=TRUE, message=FALSE, warning=FALSE-----
M <- apply(quakes, 2, mean)
SE <- apply(quakes, 2, function(x){ sd(x)/sqrt(length(x)) })
M + 1.96*SE # 95% confidence interval
M
M - 1.96*SE

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/introDA3/one-tailed-vs-two-tailed-test.jpg")

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/introDA3/hypo_error_chart.png")

## ----message = FALSE-----------------------------
library(MOTE)
M <- tapply(quakes$mag, quakes$stations, mean)
STDEV <- tapply(quakes$mag, quakes$stations, sd)
N <- tapply(quakes$mag, quakes$stations, length)

head(M)

#compare station 10 to 11
effect <- d.ind.t(M[1], M[2],
        STDEV[1], STDEV[2],
        N[1], N[2], a = .05)
effect$d

