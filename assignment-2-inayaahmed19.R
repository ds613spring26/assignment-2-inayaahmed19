# Loading libraries
library(tidyverse)
library(lubridate)

# Question 1i
pos_na_if <- function(x, y) {
  if (length(x) != length(y)) {
    return("The vectors are not the same length.")
  }
  which(is.na(x) & is.na(y))}

pos_na_if(c(1, NA, 2), c(NA, NA))
pos_na_if(c(NA, NA, 2), c(NA, NA, 2))
pos_na_if(c(NA, 5, NA), c(NA, NA, NA))
pos_na_if(c(NA, NA, NA, 2, NA, 4, NA),
          c(NA, NA, 2, 4, 4, NA, NA))
pos_na_if(c(1, NA, NA, 2, NA, 4, NA),
          c(NA, -5, 2, 4, 4, NA, 11))

# Question 1ii
pos_na_no_if <- function(x, y) {
  stopifnot(length(x) == length(y))
  which(is.na(x) & is.na(y))
}

# Question 2






# Question 3





# Question 4










