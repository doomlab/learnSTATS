## ---- include = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="25%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/descriptive.png")

## ----echo=TRUE, message=FALSE, warning=FALSE---------------------
library(datasets) 
data("sunspot.month") # special way to load embedded data
head(sunspot.month)

## ----------------------------------------------------------------
str(sunspot.month)

## ----------------------------------------------------------------
summary(sunspot.month)

## ----fig.height=3, fig.width=8, fig.align='center', message=FALSE----
library(ggplot2)
sunspot.month <- as.data.frame(sunspot.month)
sunspot.month$Time <- 1:nrow(sunspot.month)
ggplot(sunspot.month, aes(x = Time, y = x)) + 
  geom_point(alpha = 0.5) + 
  ylab("Number of Sunspots") + 
  xlab("Time") +
  theme_classic()

## ---- echo = FALSE, out.width="50%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/predictive.png")

## ----message=FALSE, warning=FALSE--------------------------------
library(quantmod)
start <- as.Date(Sys.Date()-(365*5))
end <- as.Date(Sys.Date()-2)
getSymbols("AMZN", src = "yahoo", from = start, to = end)
str(AMZN)

## ----message=FALSE, warning=FALSE--------------------------------
predictive_model <- lm(formula = AMZN.Close ~ AMZN.High + AMZN.Low + AMZN.Volume, 
                       data = AMZN[1:1199,])
summary(predictive_model)

## ----fig.height=4, fig.width=8, fig.align='center', message=FALSE, warning=FALSE----
par(mfrow=c(2,3))
plot(predictive_model,1)
plot(predictive_model,2)
plot(predictive_model,3)
plot(predictive_model,4)
plot(predictive_model,5)

## ----message=FALSE, warning=FALSE--------------------------------
n <- length(AMZN[,1])
prediction <- stats::predict(predictive_model, AMZN[1200:n,])
tail(data.frame(prediction))

## ----fig.height=4, fig.width=8, fig.align='center', message=FALSE, warning=FALSE----
plot(prediction, type = "l")

## ---- echo = FALSE, out.width="25%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/prescriptive.png")

## ---- echo = FALSE, out.width="50%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/research_process.png")

## ---- echo = FALSE, out.width="75%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/initial_obs.png")

## ---- echo = FALSE, out.width="90%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/cartoon_theory.png")

## ---- echo = FALSE, out.width="65%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/kp_false.png")

## ---- echo = FALSE, out.width="40%", fig.align='center'----------
knitr::include_graphics("pictures/introDA1/cross_sectional_research_study.png")

## ----------------------------------------------------------------
tapply(iris$Sepal.Length, iris$Species, mean)

## ----------------------------------------------------------------
sample <- iris[sample(nrow(iris), 15), ]
tapply(sample$Sepal.Length, sample$Species, mean) #sample
tapply(iris$Sepal.Length, iris$Species, mean) #population

