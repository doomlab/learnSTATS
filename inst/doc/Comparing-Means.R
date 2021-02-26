## ---- include = FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(rio)
longdata <- import("data/invisible.csv")
str(longdata)

## -------------------------------------------------------------------------------
M <- tapply(longdata$Mischief, longdata$Cloak, mean)
STDEV <- tapply(longdata$Mischief, longdata$Cloak, sd)
N <- tapply(longdata$Mischief, longdata$Cloak, length)

M;STDEV;N

## ---- echo = FALSE, out.width="50%", fig.align='center'-------------------------
knitr::include_graphics("pictures/ttests/1.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, #assume equal variances
       paired = FALSE) #independent

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = FALSE, #assume equal variances
       paired = FALSE) #independent

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(MOTE)
effect <- d.ind.t(m1 = M[1], m2 = M[2],        
                  sd1 = STDEV[1], sd2 = STDEV[2],        
                  n1 = N[1], n2 = N[2], a = .05)
effect$d

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(pwr)
pwr.t.test(n = NULL, #leave NULL
           d = effect$d, #effect size
           sig.level = .05, #alpha
           power = .80, #power 
           type = "two.sample", #independent
           alternative = "two.sided") #two tailed test

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, #ignored in dependent t
       paired = TRUE) #dependent t

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
effect2 <- d.dep.t.avg(m1 = M[1], m2 = M[2],
                       sd1 = STDEV[1], sd2 = STDEV[2],  
                       n = N[1], a = .05)
effect2$d

#remember independent t
effect$d

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
diff <- longdata$Mischief[longdata$Cloak == "Cloak"] - longdata$Mischief[longdata$Cloak == "No Cloak"]

effect2.1 = d.dep.t.diff(mdiff = mean(diff, na.rm = T), 
                         sddiff = sd(diff, na.rm = T),
                         n = length(diff), a = .05)
effect2.1$d

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
pwr.t.test(n = NULL, 
           d = effect2$d, 
           sig.level = .05,
           power = .80, 
           type = "paired", 
           alternative = "two.sided")

## ----echo=TRUE, message=FALSE, warning=FALSE------------------------------------
library(ggplot2)

cleanup <- theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph <- ggplot(longdata, aes(Cloak, Mischief))

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
  xlab("Invisible Cloak Group") +
  ylab("Average Mischief Acts")

