## ---- include = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="100%", fig.align='left'-----------
knitr::include_graphics("pictures/regression/5.png")

## ---- echo = FALSE, out.width="85%", fig.align='left'------------
knitr::include_graphics("pictures/regression/7.png")

## ---- echo = FALSE, out.width="100%", fig.align='left'-----------
knitr::include_graphics("pictures/regression/6.png")

## ----------------------------------------------------------------
library(rio)
master <- import("data/regression_data.sav")
master <- master[ , c(8:11)]
str(master)

## ----------------------------------------------------------------
summary(master)
nomiss <- na.omit(master)
nrow(master)
nrow(nomiss)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
mahal <- mahalanobis(nomiss, 
                    colMeans(nomiss), 
                    cov(nomiss))
cutmahal <- qchisq(1-.001, ncol(nomiss))
badmahal <- as.numeric(mahal > cutmahal) ##note the direction of the > 
table(badmahal)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model1 <- lm(CESD_total ~ PIL_total + AUDIT_TOTAL_NEW + DAST_TOTAL_NEW, 
             data = nomiss)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
k = 2 ##number of IVs
leverage <- hatvalues(model1)
cutleverage <- (2*k+2) / nrow(nomiss)
badleverage <- as.numeric(leverage > cutleverage)
table(badleverage)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
cooks <- cooks.distance(model1)
cutcooks <- 4 / (nrow(master) - k - 1)
badcooks <- as.numeric(cooks > cutcooks)
table(badcooks)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
##add them up!
totalout <- badmahal + badleverage + badcooks
table(totalout)

noout <- subset(master, totalout < 2)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model2 <- lm(CESD_total ~ PIL_total + AUDIT_TOTAL_NEW + DAST_TOTAL_NEW, 
             data = noout)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
summary(model2, correlation = TRUE)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
standardized <- rstudent(model2)
fitted <- scale(model2$fitted.values)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
{qqnorm(standardized)
abline(0,1)}

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
hist(standardized)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
{plot(fitted, standardized)
abline(0,0)
abline(v = 0)}

## ----------------------------------------------------------------
summary(model2)

library(papaja)
apa_style <- apa_print(model2)
apa_style$full_result$modelfit

## ----------------------------------------------------------------
summary(model2)

## ----------------------------------------------------------------
apa_style$full_result$PIL_total
apa_style$full_result$AUDIT_TOTAL_NEW
apa_style$full_result$DAST_TOTAL_NEW

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(QuantPsyc)
lm.beta(model2)

## ---- echo = FALSE, out.width="50%", fig.align='center'----------
knitr::include_graphics("pictures/regression/19.png")

## ---- echo = FALSE, out.width="50%", fig.align='center'----------
knitr::include_graphics("pictures/regression/19.png")

## ---- echo = FALSE, out.width="50%", fig.align='center'----------
knitr::include_graphics("pictures/regression/19.png")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(ppcor)
partials <- pcor(noout)
partials$estimate^2

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/regression/21.png")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
hdata <- import("data/dummy_code.sav")
str(hdata)

## ----------------------------------------------------------------
attributes(hdata$treat)
hdata$treat <- factor(hdata$treat,
                     levels = 0:4,
                     labels = c("No Treatment", "Placebo", "Paxil",
                                "Effexor", "Cheerup"))

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model1 <- lm(after ~ familyhistory, data = hdata)
summary(model1)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model2 <- lm(after ~ familyhistory + treat, data = hdata)
summary(model2)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
anova(model1, model2)

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
summary(model2)

library(emmeans)
emmeans(model2, "treat")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model_summary <- summary(model2)
t_values <- model_summary$coefficients[ , 3] 
df_t <- model_summary$df[2]

t_values^2 / (t_values^2+df_t)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(pwr)
R2 <- model_summary$r.squared
f2 <- R2 / (1-R2)

R2
f2

## ----------------------------------------------------------------
#f2 is cohen f squared 
pwr.f2.test(u = model_summary$df[1], 
            v = NULL, f2 = f2, 
            sig.level = .05, power = .80)

