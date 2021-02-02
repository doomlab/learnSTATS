## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, message = FALSE, warning = FALSE-----
library(rio)
master <- import("data/data_screening.csv")
notypos <- master #update the dataset with each step 
notypos$Sex <- factor(notypos$Sex, 
                     levels = c(1,2), #no 3
                     labels = c("Women", "Men"))
notypos$SES <- factor(notypos$SES, 
                     levels = c(1,2, 3),
                     labels = c("Low", "Medium", "High"))
notypos$Grade[ notypos$Grade > 15 ] <- NA
notypos[ , 6:19][ notypos[ , 6:19] > 7 ] <- NA
percentmiss <- function(x){ sum(is.na(x))/length(x) * 100 }
missing <- apply(notypos, 1, percentmiss)
replace_rows <- subset(notypos, missing <= 5) #5%
noreplace_rows <- subset(notypos, missing > 5)
replace_columns <- replace_rows[ , -c(1,2,4)]
noreplace_columns <- replace_rows[ , c(1,2,4)] #notice these are both replace_rows
library(mice)
temp_no_miss <- mice(replace_columns)
nomiss <- complete(temp_no_miss, 1) #pick a dataset 1-5 
all_columns <- cbind(noreplace_columns, nomiss)
all_rows <- rbind(noreplace_rows, all_columns)
mahal <- mahalanobis(all_columns[ , -c(1,4)],
                    colMeans(all_columns[ , -c(1,4)], na.rm=TRUE),
                    cov(all_columns[ , -c(1,4)], use ="pairwise.complete.obs"))
cutoff <- qchisq(1-.001, ncol(all_columns[ , -c(1,4)]))
noout <- subset(all_columns, mahal < cutoff)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
str(noout)
cor(noout[ , -c(1,4)])

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(corrplot)
corrplot(cor(noout[ , -c(1,4)]))

## -------------------------------------------------------
random <- rchisq(nrow(noout), 7) #why 7? It works, any number bigger than 2
fake <- lm(random ~ ., #Y is predicted by all variables in the data
           data = noout) #You can use categorical variables now!
standardized <- rstudent(fake)
fitvalues <- scale(fake$fitted.values)

## -------------------------------------------------------
{qqnorm(standardized)
abline(0,1)}

plot(fake, 2)

## -------------------------------------------------------
{qqnorm(standardized)
abline(0,1)}

## ----echo=TRUE, message=FALSE, warning=FALSE------------
hist(noout$RS1)
library(moments)
skewness(noout[ , -c(1,4)])
kurtosis(noout[ , -c(1,4)]) - 3 #to get excess kurtosis

## ----echo=TRUE, message=FALSE, warning=FALSE------------
hist(standardized, breaks=15)
length(standardized)

## ---- echo = FALSE, out.width="100%", fig.align='center'----
knitr::include_graphics("pictures/datascreen/2.png")

## ---- echo = FALSE, out.width="100%", fig.align='center'----
knitr::include_graphics("pictures/datascreen/3.png")

## -------------------------------------------------------
{plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)}

## -------------------------------------------------------
{plot(fitvalues, standardized) 
abline(0,0)
abline(v = 0)}

