## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/correl/1.png")

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/correl/2.png")

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/correl/3.png")

## -------------------------------------------------------
library(rio)
library(ggplot2)
cleanup <- theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))
exam <- import("data/exam_data.csv")
liar <- import("data/liar_data.csv")

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
#from chapter 5 notes
scatter <- ggplot(exam, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
#from chapter 5 notes + coord_cartesian
scatter <- ggplot(exam, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup + 
  coord_cartesian(xlim = c(50,100), ylim = c(0,100))
  #just example numbers, you would want to use the real scale of the data

## -------------------------------------------------------
var(exam$Revise)
var(exam$Exam)

## -------------------------------------------------------
cov(exam$Revise, exam$Exam)
plot(exam$Revise, exam$Exam)

## -------------------------------------------------------
cor(exam$Revise, exam$Exam)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
cor(exam[ , -1], 
    use="pairwise.complete.obs", 
    method = "pearson")

cor(exam[ , -1], 
    use="pairwise.complete.obs", 
    method = "kendall")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(Hmisc)
rcorr(as.matrix(exam[ , -1]), type = "pearson")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
cor.test(exam$Revise,
         exam$Exam,
         method = "pearson")

## -------------------------------------------------------
str(liar)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
with(liar, cor.test(Creativity, Position, method = "spearman"))

## ----echo=TRUE, message=FALSE, warning=FALSE------------
with(liar, cor.test(Creativity, Position, method = "kendall"))

## ----echo=TRUE, message=FALSE, warning=FALSE------------
liar$Novice2 <- as.numeric(as.factor(liar$Novice))
str(liar) #we had to factor because of the character variable
with(liar, cor.test(Creativity, Novice2))
plot(liar$Creativity, liar$Novice2)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(cocor)
new <- subset(liar, Novice == "First Time")
old <- subset(liar, Novice == "Had entered Competition Before")
ind_data <- list(new, old)
cocor(~Creativity + Position | Creativity + Position,
      data = ind_data)

## -------------------------------------------------------
cocor(~Revise + Exam | Revise + Anxiety, 
      data = exam)

## ---- echo = FALSE, out.width="75%", fig.align='center'----
knitr::include_graphics("pictures/correl/9.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(ppcor)
pcor(exam[ , -c(1)], method = "pearson")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
spcor(exam[ , -c(1)], method = "pearson")

