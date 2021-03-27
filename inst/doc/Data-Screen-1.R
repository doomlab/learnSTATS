## ---- include = FALSE--------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------
library(rio)
master <- import("data/data_screening.csv")
str(master)

## ----------------------------------------------------------
notypos <- master #update the dataset with each step 
apply(notypos[ , c("Sex", "SES")], 2, table)
#3 here for sex is probably incorrect

## ----------------------------------------------------------
## fix the categorical labels and typos
notypos$Sex <- factor(notypos$Sex, 
                     levels = c(1,2), #no 3
                     labels = c("Women", "Men"))
notypos$SES <- factor(notypos$SES, 
                     levels = c(1,2, 3),
                     labels = c("Low", "Medium", "High"))
apply(notypos[ , c("Sex", "SES")], 2, table)

## ----------------------------------------------------------
summary(notypos)

## ----------------------------------------------------------
summary(notypos$Grade)
notypos$Grade[ notypos$Grade > 12 ] <- NA
summary(notypos$Grade)

summary(notypos$Absences)
notypos$Absences[ notypos$Absences > 15 ] <- NA
summary(notypos$Absences)

## ----------------------------------------------------------
names(notypos)
head(notypos[ , 6:19]) #lots of ways to do this part!
notypos[ , 6:19][ notypos[ , 6:19] > 7 ] <- NA
summary(notypos)

## ----------------------------------------------------------
names(notypos)
apply(notypos[ , -c(1,3)], 2, mean, na.rm = T)
apply(notypos[ , -c(1,3)], 2, sd, na.rm = T)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------
summary(notypos)
apply(notypos, 2, function(x) { sum(is.na(x)) })

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/datascreen/missing.png")

## ----------------------------------------------------------
library(VIM, quietly = T)
aggr(notypos, numbers = T)

## ----------------------------------------------------------
percentmiss <- function(x){ sum(is.na(x))/length(x) * 100 }
missing <- apply(notypos, 1, percentmiss)
table(missing)

## ----------------------------------------------------------
replace_rows <- subset(notypos, missing <= 5) #5%
noreplace_rows <- subset(notypos, missing > 5)

nrow(notypos)
nrow(replace_rows)
nrow(noreplace_rows)

## ----------------------------------------------------------
apply(replace_rows, 2, percentmiss)

## ----------------------------------------------------------
replace_columns <- replace_rows[ , -c(1,2,4)]
noreplace_columns <- replace_rows[ , c(1,2,4)] #notice these are both replace_rows

## ----------------------------------------------------------
library(mice)
temp_no_miss <- mice(replace_columns)

## ----echo=TRUE, message=FALSE, warning=FALSE---------------
nomiss <- complete(temp_no_miss, 1) #pick a dataset 1-5 

#combine back together
dim(notypos) #original data from previous step
dim(nomiss) #replaced data

#get all columns 
all_columns <- cbind(noreplace_columns, nomiss)
dim(all_columns)

#get all rows
all_rows <- rbind(noreplace_rows, all_columns)
dim(all_rows)

## ----message=FALSE, warning=FALSE--------------------------
## you can use all columns or all rows here
## however, all rows has missing data, which will not get a score 
str(all_columns)
mahal <- mahalanobis(all_columns[ , -c(1,4)],
                    colMeans(all_columns[ , -c(1,4)], na.rm=TRUE),
                    cov(all_columns[ , -c(1,4)], use ="pairwise.complete.obs"))

## ----------------------------------------------------------
## remember to match the number of columns
cutoff <- qchisq(1-.001, ncol(all_columns[ , -c(1,4)]))

## df and cutoff
ncol(all_columns[ , -c(1,4)])
cutoff

##how many outliers? Look at FALSE
summary(mahal < cutoff)

## eliminate
noout <- subset(all_columns, mahal < cutoff)
dim(all_columns)
dim(noout)

