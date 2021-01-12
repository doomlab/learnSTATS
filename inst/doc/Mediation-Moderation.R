## ---- include = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/medmod/1.png")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(rio)
master <- import("data/mediation.sav")
str(master)

summary(master)
master <- na.omit(master)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model1 <- lm(exam ~ previous, data = master)
summary(model1)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model2 <- lm(facebook ~ previous, data = master)
summary(model2)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
model3 <- lm(exam ~ previous + facebook, data = master)
summary(model3)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
#aroian sobel
a <- coef(model2)[2]
b <- coef(model3)[3]
SEa <- summary(model2)$coefficients[2,2]
SEb <- summary(model3)$coefficients[3,2]
zscore <- (a*b)/(sqrt((b^2*SEa^2)+(a^2*SEb^2)+(SEa^2*SEb^2)))
zscore
#two tailed test 
pnorm(abs(zscore), lower.tail = F)*2

## ----------------------------------------------------------------
#devtools::install_github("doomlab/MeMoBootR")
library(MeMoBootR)

#no missing data allowed
med_results <- mediation1(y = "exam",
                          x = "previous", 
                          m = "facebook", 
                          df = master)

## ----------------------------------------------------------------
head(med_results$datascreening$fulldata)
med_results$datascreening$correl
med_results$datascreening$linearity
med_results$datascreening$normality
med_results$datascreening$homogen

## ----------------------------------------------------------------
summary(med_results$model1)
summary(med_results$model2)
summary(med_results$model3)

## ----------------------------------------------------------------
med_results$indirect.effect
med_results$z.score
med_results$p.value

## ----------------------------------------------------------------
med_results$boot.results
med_results$boot.ci
med_results$diagram

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/medmod/4.png")

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/medmod/5.png")

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/medmod/6.png")

## ---- echo = FALSE, out.width="75%", fig.align='left'------------
knitr::include_graphics("pictures/medmod/7.png")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
master <- import("data/moderation.sav")
str(master)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
#center the X and M variable (NOT THE DV)
#when you create these use the final dataset 
master$zvid = scale(master$Vid_Games, scale = F) #mean center, not z score
master$zcal = scale(master$CaUnTs, scale = F)

## ----message=FALSE, warning=FALSE--------------------------------
#run the model to see if the moderation is significant
#use the z score variables!
#be sure to do X*M so the graph is right 
modmodel <- lm(Aggression ~ zvid*zcal, data = master)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
summary(modmodel)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
#create the low and high z score variables 
master$zcallow <- master$zcal + sd(master$zcal) #bring them up
master$zcalhigh <- master$zcal - sd(master$zcal) #bring them down
summary(master)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
#run the models
#be sure to X*M
modmodellow <- lm(Aggression ~ zvid*zcallow, data = master)
modmodelhigh <- lm(Aggression ~ zvid*zcalhigh, data = master)


## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
summary(modmodellow) #low slope

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
summary(modmodel) #average slope

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
summary(modmodelhigh) #high slope

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(ggplot2)
cleanup <- theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

modgraph <- ggplot(master, aes(zvid, Aggression))

##change Cal to the new moderator label
##change xlab for the new X label
modgraph + 
  xlab("Centered Video Games") + 
  geom_point(color = "gray") +
  geom_abline(aes(intercept = modmodellow$coefficients[1],
                  slope = modmodellow$coefficients[2], 
                  linetype = "-1SD Cal"), size = 1) +
  geom_abline(aes(intercept = modmodel$coefficients[1],
                  slope = modmodel$coefficients[2], 
                  linetype = "Average Cal"), size = 1) +
  geom_abline(aes(intercept = modmodelhigh$coefficients[1],
                  slope = modmodelhigh$coefficients[2], 
                  linetype = "+1SD Cal"), size = 1) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD Cal", "Average Cal", "+1SD Cal"),
                        name = "Simple Slope") +
  cleanup 

## ----------------------------------------------------------------
mod_model <- moderation1(y = "Aggression",
                         x = "Vid_Games",
                         m = "CaUnTs",
                         df = master)

## ----------------------------------------------------------------
#data screening
head(mod_model$datascreening$fulldata)
#mod_model$datascreening$correl
#mod_model$datascreening$linearity
#mod_model$datascreening$normality
#mod_model$datascreening$homogen

#models
summary(mod_model$model1)
#summary(mod_model$model1low)
#summary(mod_model$model1high)
mod_model$interpretation

#graphs
mod_model$graphslopes

