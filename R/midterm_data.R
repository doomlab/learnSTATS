#' Exam Data Set Up
#'
#' This function grabs the midterm/final data,
#' randomizes it for each student,
#' and allows them to each have a unique dataset.
#'
#' @param id Student ID number to set up data.
#'
#' @return midterm dataset
#'
#' @keywords student answers, datasets
#' @import faux
#' @export

midterm_data <- function(idnum = NULL) {

  if (is.null(idnum)){
    warning("You must put in an id number.")
  } else {
    data("midterm")
    midterm <- na.omit(midterm)

    set.seed(idnum)

    suppressPackageStartupMessages(require(faux, quietly = T))
    midterm <- sim_df(data = midterm, #data frame
                      n = sample(50:100, 1),#how many of each group
                      between = c("JOL_group", "type_cue"))
    midterm <- midterm[ , -1]

    midterm <- messy(midterm,
                     prop = .03,
                     2:22,
                     replace = NA)

    return(midterm)
    }

}

#' @rdname midterm_data
#' @export
