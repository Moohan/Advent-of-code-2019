library(readr)
library(dplyr)
library(stringr)

input <- read_lines("data/day3") %>%  str_split(",")

wire1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
wire2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")

get_points <- function(wire) {
  points <- matrix(c(0,0), ncol = 2)
  for (movements in str_match_all(wire, "([LRUD])(\\d\\d?\\d?)")) {
    direction <- case_when(movements[,2] == "L" ~
    distance <- movements[,3]
    for (steps in 1:distance) {
      points <- rbind()
    }

  }
}



