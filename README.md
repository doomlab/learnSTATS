## learnSTATS

`learnSTATS` is a tutorial package for a graduate introduction to statistics course written by Erin M. Buchanan at https://statisticsofdoom.com/. The course is situated in an Analytics program and covers an introduction to statistical concepts in *R*. 

Current Version: `0.1.0`

### Installation

You can install `learnSTATS` by using the following code:

```
#install.packages("devtools") #uncomment if you need devtools
library(devtools)
install_github("doomlab/learnSTATS")
```

Be sure to restart your **R** session, as this helps you get the **Tutorial Window** from the `learnr` package. If you see a message *no tutorial found in learnSTATS*, try restarting RStudio. 

### Course Schedule

1. Introduction to R: 

- Lecture: `vignette("Introduction-to-R", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("introR", "learnSTATS")`

2. Introduction to Data Analytics

- Lecture: `vignette("Introduction-Data-Analytics", "learnSTATS")`

3. Basic Statistics

- Lecture: `vignette("Basic-Stats-Concepts", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("basics", "learnSTATS")`

4. More Statistics

- Lecture: `vignette("More-Stats-Concepts", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("moreStats", "learnSTATS")`

5. Data Visualization

- Lecture: `vignette("Data-Visualization", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("datavis", "learnSTATS")`

6. Data Screening Part 1

- Lecture: `vignette("Data-Screen-1", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("datascreen", "learnSTATS")`

7. Data Screening Part 2

- Lecture: `vignette("Data-Screen-2", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("datascreen2", "learnSTATS")`

8. Correlation

- Lecture: `vignette("Correlation", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("correlation", "learnSTATS")`

9. Regression

- Lecture: `vignette("Linear-Regression", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("regression", "learnSTATS")`

10. Mediation and Moderation

- Lecture: `vignette("Mediation-Moderation", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("medmod", "learnSTATS")` 

11. t-Tests

- Lecture: `vignette("Comparing-Means", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("ttests", "learnSTATS")` 

12. ANOVA

- Lecture: `vignette("ANOVA", "learnSTATS")`
- Tutorial: `learnr::run_tutorial("anova", "learnSTATS")`

Lecture videos are embedded directly into the tutorials and the notes are provided as vignette. 
