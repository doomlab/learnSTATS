## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = F-------------------------------------------
#  textline +
#    stat_summary(fun.y = mean,
#                 geom = "point") +
#    stat_summary(fun.y = mean,
#                 geom = "line",
#                 aes(group = Group)) +
#    stat_summary(fun.data = mean_cl_normal,
#                 geom = "errorbar",
#                 width = .2) +
#    xlab("Measurement Time") +
#    ylab("Mean Grammar Score") +
#    cleanup +
#    scale_color_manual(name = "Texting Option",
#                       labels = c("All the texts", "None of the texts"),
#                       values = c("Black", "Grey")) +
#    scale_x_discrete(labels = c("Baseline", "Six Months"))

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(rio)
chickflick <- import("data/ChickFlick.sav")
str(chickflick)

## -------------------------------------------------------
table(chickflick$gender)

table(chickflick$film)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
chickflick$gender <- factor(chickflick$gender, #the variable you want to factor
                            levels = c(1,2), #the information already in the data
                            labels = c("Male", "Female")) #the labels for those levels

table(chickflick$gender)

## -------------------------------------------------------
library(reshape) #note: you could also use pivot_longer in tidyverse
cricket <- import("data/Jiminy_Cricket.csv")
head(cricket)

## -------------------------------------------------------
longcricket <- melt(cricket,  #name of dataset
                    id = c("ID", "Strategy"), 
                    measured = c("Success_Pre", "Success_Post"))
#you can actually leave measured blank
head(longcricket)

## -------------------------------------------------------
colnames(longcricket)[3:4] #just to figure out which ones
colnames(longcricket)[3:4] <- c("Time", "Score")

## ---- echo = FALSE, out.width="35%", fig.align='center'----
knitr::include_graphics("pictures/graphs/badgraph.png")

## ---- echo = FALSE, out.width="35%", fig.align='center'----
knitr::include_graphics("pictures/graphs/bettergraph.png")

## ---- echo = FALSE, out.width="50%", fig.align='center'----
knitr::include_graphics("pictures/graphs/deception.png")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
library(ggplot2)

## ----eval = F-------------------------------------------
#  #an example
#  myGraph <- ggplot(dataset,
#                    aes(x_axis, y_axis,
#                        color = legend_var,
#                        fill = legend_var))

## ----eval = F-------------------------------------------
#  #an example part 2
#  myGraph +
#    geom_bar() +
#    geom_point() +
#    xlab("X Axis Label") +
#    ylab("Y Axis Label")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
crickethist <- ggplot(data = cricket, #dataset
                      aes(x = Success_Pre) #only define X axis 
                      )
crickethist

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
crickethist + 
  geom_histogram()

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
crickethist + 
  geom_histogram(binwidth = 1)

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
crickethist + 
  geom_histogram(binwidth = 1, color = 'purple', fill = 'magenta')

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
crickethist + 
  geom_histogram(binwidth = 1, color = 'purple', fill = 'magenta') + 
  xlab("Success Pre Test") + 
  ylab("Frequency")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
festival <- import("data/festival.csv")
str(festival)

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
festivalhist <- ggplot(data = festival, aes(x = day1)) 
festivalhist + 
  geom_histogram(binwidth = 1, color = 'blue') + 
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  theme_bw() #theme_classic() also good!

## ----echo=TRUE, message=FALSE, warning=FALSE------------
cleanup <- theme(panel.grid.major = element_blank(), #no grid lines
                panel.grid.minor = element_blank(), #no grid lines
                panel.background = element_blank(), #no background
                axis.line.x = element_line(color = 'black'), #black x axis line
                axis.line.y = element_line(color = 'black'), #black y axis line
                legend.key = element_rect(fill = 'white'), #no legend background
                text = element_text(size = 15)) #bigger text size

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
festivalhist + 
  geom_histogram(binwidth = 1, color = 'blue') + 
  xlab("Day 1 of Festival Hygiene") +
  ylab("Frequency") +
  cleanup

## ----echo=TRUE, message=FALSE, warning=FALSE------------
exam <- import("data/Exam_Anxiety.csv")
str(exam)

## -------------------------------------------------------
table(exam$Gender)
exam$Gender <- factor(exam$Gender,
                     levels = c(1,2),
                     labels = c("Male", "Female"))
table(exam$Gender)

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
scatter <- ggplot(exam, aes(Anxiety, Exam))
scatter +
  geom_point() +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup

## ----echo=TRUE,  fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
scatter + geom_point()+
  geom_smooth(method = 'lm', color = 'black', fill = 'blue') +
  xlab('Anxiety Score')+
  ylab('Exam Score')+
  cleanup

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
scatter2 <- ggplot(exam, aes(Anxiety, Exam, 
                             color = Gender, fill = Gender)) #why both?
scatter2 +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Anxiety Score") +
  ylab("Exam Score") +
  cleanup + 
  scale_fill_manual(name = "Gender of Participant",
                    labels = c("Men", "Women"),
                    values = c("purple", "grey")) +
  scale_color_manual(name = "Gender of Participant",
                     labels = c("Men", "Women"),
                     values = c("purple", "grey10"))


## -------------------------------------------------------
library(GGally)
ggpairs(data = exam[ , -1], #no participant variable
        title = "Exam Anxiety, Scores, and Gender")

## ----echo=TRUE, message=FALSE, warning=FALSE------------
str(chickflick) #already fixed gender
chickflick$film <- factor(chickflick$film,
                    levels = c(1,2),
                    labels = c("Bridget Jones", "Memento"))

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
chickbar <- ggplot(chickflick, aes(film, arousal))
chickbar + 
  stat_summary(fun = mean,
               geom = "bar",
               fill = "White", 
               color = "Black") +
  cleanup

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
chickbar + 
  stat_summary(fun = mean,
               geom = "bar",
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) +
  cleanup

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
chickbar + 
  stat_summary(fun.y = mean,
               geom = "bar",
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90), 
               width = 0.2) +
  xlab("Movie Watched by Participant") +
  ylab("Arousal Level") +
  cleanup +
  scale_x_discrete(labels = c("Girl Film", "Guy Film"))


## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
chickbar2 <- ggplot(chickflick, aes(film, arousal, fill = gender))
chickbar2 +
  stat_summary(fun.y = mean,
               geom = "bar",
               position = "dodge") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = .2) +
  xlab("Film Watched") +
  ylab("Arousal Level") + 
  cleanup +
  scale_fill_manual(name = "Gender of Participant", 
                    labels = c("Boys", "Girls"),
                    values = c("Gray30", "Gray"))

## ----echo=TRUE, message=FALSE, warning=FALSE------------
hiccups <- import("data/Hiccups.csv")
str(hiccups)

## ----echo=TRUE, message=FALSE, warning=FALSE------------
longhiccups <- melt(hiccups, 
                    measured = c("Baseline", "Tongue", "Carotid", "Other"))
str(longhiccups)
colnames(longhiccups) <- c("Intervention", "Hiccups")

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
hiccupline <- ggplot(longhiccups, aes(Intervention, Hiccups))
hiccupline +
  stat_summary(fun = mean, ##adds the points
               geom = "point") +
  stat_summary(fun = mean, ##adds the line
               geom = "line",
               aes(group=1)) + ##necessary for mapping line to dots
  stat_summary(fun.data = mean_cl_normal, ##adds the error bars
               geom = "errorbar", 
               width = .2) +
  xlab("Intervention Type") +
  ylab("Number of Hiccups") + 
  cleanup

## ----echo=TRUE, message=FALSE, warning=FALSE------------
texting <- import("data/Texting.xlsx")
str(texting)

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
texting$Group <- factor(texting$Group,
                       levels = c(1,2),
                       labels = c("Texting Allowed", "No Texting Allowed"))
longtexting <- melt(texting,
                   id = c("Group"),
                   measured = c("Baseline", "Six_months"))
str(longtexting)
colnames(longtexting) <- c("Group", "Time", "Grammar_Score")

## ----echo=TRUE, fig.height=4, fig.width=8, message=FALSE, warning=FALSE----
textline <- ggplot(longtexting, aes(Time, Grammar_Score, color = Group))
textline +
  stat_summary(fun = mean,
               geom = "point") +
  stat_summary(fun = mean,
               geom = "line", 
               aes(group = Group)) + #Group is the variable name
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = .2) + 
  xlab("Measurement Time") +
  ylab("Mean Grammar Score") +
  cleanup +
  scale_color_manual(name = "Texting Option",
                     labels = c("All the texts", "None of the texts"),
                     values = c("Black", "Grey")) +
  scale_x_discrete(labels = c("Baseline", "Six Months"))

