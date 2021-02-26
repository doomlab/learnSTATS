## ---- include = FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="75%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/1.png")

## ----message=FALSE--------------------------------------------------------------
library(rio)
master <- import("data/viagra.sav")
str(master)
master$dose <- factor(master$dose, 
                      levels = c(1,2,3),
                      labels = c("Placebo", "Low Dose", "High Dose"))

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/4.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/5.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/6.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/7.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/8.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/9.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/10.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/11.png")

## ---- echo = FALSE, out.width="50%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/12.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/13.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/14.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'------------------------
knitr::include_graphics("pictures/anova/15.png")

## ----echo=TRUE------------------------------------------------------------------
library(ez)

## you must have a participant number for ezANOVA
master$partno <- 1:nrow(master)
options(scipen = 20)
ezANOVA(data = master,
        dv = libido,
        between = dose,
        wid = partno,
        type = 3, 
        detailed = T)

## ----echo = T, message=FALSE, warning=FALSE-------------------------------------
ezANOVA(data = master,
        dv = libido,
        between = dose,
        wid = partno,
        type = 3, 
        detailed = T)$`Levene's Test for Homogeneity of Variance`

## -------------------------------------------------------------------------------
## running a one way anova - if Levene's Test is significant
oneway.test(libido ~ dose, data = master)

## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------
ezANOVA(data = master,
        dv = libido,
        between = dose,
        wid = partno,
        type = 3, 
        detailed = T)$ANOVA

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(MOTE)
effect <- omega.F(dfm = 2, #this is dfn in the anova
                  dfe = 12, #this is dfd in the anova
                  Fvalue = 5.12, #this is F
                  n = 15, #look at the number of rows in your dataset
                  a = .05) #leave this as .05
effect$omega

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
## post hoc tests - p.value adjustment "none"
pairwise.t.test(master$libido,
                master$dose,
                p.adjust.method = "none", 
                paired = F, 
                var.equal = T)

## ---- echo = FALSE, out.width="75%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/16.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
## post hoc tests - p.value adjustment "bonferroni"
pairwise.t.test(master$libido,
                master$dose,
                p.adjust.method = "bonferroni", 
                paired = F, 
                var.equal = T)

## ---- echo = FALSE, out.width="75%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/17.png")

## ---- echo = FALSE, out.width="75%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/18.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
## get numbers for effect size
M <- tapply(master$libido, master$dose, mean)
N <- tapply(master$libido, master$dose, length)
STDEV <- tapply(master$libido, master$dose, sd)

## placebo to low
effect1 <- d.ind.t(m1 = M[1], m2 = M[2],
                 sd1 = STDEV[1], sd2 = STDEV[2],
                 n1 = N[1], n2 = N[2], a = .05)
effect1$d

## placebo to high
effect2 = d.ind.t(m1 = M[1], m2 = M[3],
                 sd1 = STDEV[1], sd2 = STDEV[3],
                 n1 = N[1], n2 = N[3], a = .05)
effect2$d

## low to high
effect3 = d.ind.t(m1 = M[2], m2 = M[3],
                  sd1 = STDEV[2], sd2 = STDEV[3],
                  n1 = N[2], n2 = N[3], a = .05)
effect3$d

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE---------
library(ggplot2)

cleanup <- theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph <- ggplot(master, aes(dose, libido))
bargraph +
  cleanup +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  xlab("Dosage of Drug") +
  ylab("Average Libido")

## ---- echo = FALSE, out.width="75%", fig.align='center'-------------------------
knitr::include_graphics("pictures/anova/19.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
## trend analysis
k = 3 ## set to the number of groups
master$dose2 <- master$dose #this changes the variable
contrasts(master$dose2) <- contr.poly(k) ## note this does change the original dataset
output <- aov(libido ~ dose2, data = master)
summary.lm(output)

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE---------
doseline <- ggplot(master, aes(dose2, libido))
doseline +
  stat_summary(fun.y = mean, ## adds the points
               geom = "point") +
  stat_summary(fun.y = mean, ## adds the line
               geom = "line",
               aes(group=1)) +
  stat_summary(fun.data = mean_cl_normal, ## adds the error bars
               geom = "errorbar", 
               width = .2) +
  xlab("Dose of Viagra") +
  ylab("Average Libido") + 
  cleanup

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(pwr)
eta = .46
f_eta = sqrt(eta / (1-eta))

pwr.anova.test(k = 3, #levels 
               n = NULL, #leave to get sample size per group
               f = f_eta, #f from eta
               sig.level = .05, #alpha
               power = .80) #power

