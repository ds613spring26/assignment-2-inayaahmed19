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
library(readxl)
wmata <- read_csv("https://dcgerard.github.io/stat_412_612/data/wmata_ridership.csv")

wmata |>
  mutate(date = as.Date(Date))

wmata <- wmata |>
  mutate(year = lubridate::year(Date),
    month = lubridate::month(Date),
    day = lubridate::day(Date))

wmata_prop <- wmata |>
  group_by(year, month) |>
  mutate(prop = Total / sum(Total)) |>
  ungroup()

wmata_prop

ggplot(wmata_prop, aes(x = factor(day), y = prop)) +
  geom_boxplot() +
  labs(
    x = "Day of Month",
    y = "Proportion of Monthly Rides",
    title = "Daily Ride Proportions by Day of Month")

# Question 3
V <- c("Bears", "Lions", "Dolphins", "Eagles", "Bengals")
V

V[c(1, 3, 5)]


# Question 4










