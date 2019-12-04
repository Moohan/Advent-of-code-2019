library(readr)
library(dplyr)
library(purrr)
library(stringr)

input <- read_lines("data/day3") %>% str_split(",")

add_points <- function(points, movement) {
  current_point <- points[nrow(points), ]
  direction <- case_when(movement[, 2] == "L" ~ c(-1, 0),
                         movement[, 2] == "R" ~ c(1, 0),
                         movement[, 2] == "U" ~ c(0, 1),
                         movement[, 2] == "D" ~ c(0, -1))
  distance <- movement[, 3]

  for (step in 1:distance) {
    points <- points %>% rbind(c(step * direction) + current_point)
  }

  return(points)
}

get_points <- function(wire) {
  points <- matrix(c(0,0), ncol = 2)
  for (movement in str_match_all(wire, "([LRUD])(\\d+)")) {
    points <- points %>% add_points(movement)
  }

  return(points)
}

manhattan_dist <- function(point1, point2) {
  distance <- abs(point1[1] - point2[1]) + abs(point1[2] - point2[2])
  return(as.integer(distance))
}

closest_cross_distance <- function(wire1, wire2) {
  wire1_points <- get_points(wire1)
  wire2_points <- get_points(wire2)

  cross_points <- merge(wire1_points, wire2_points)[-1, ]

  distance <- 1:nrow(cross_points) %>%
    map_dbl( ~ manhattan_dist(cross_points[., ], c(0,0))) %>%
    min()

  return(distance)
}

#closest_cross_distance(input[[1]], input[[2]])


# part 2 ------------------------------------------------------------------

distance_to_cross <- function(wire1_points, wire2_points, cross_point) {
  points_list1 <- split(wire1_points, rep(1:nrow(wire1_points)))
  points_list2 <- split(wire2_points, rep(1:nrow(wire2_points)))

  distance <- sum(which(map_lgl(points_list1, ~ all(. == cross_point))),
    which(map_lgl(points_list2, ~ all(. == cross_point))))
  return(distance)
}

wire1_points <- get_points(input[[1]])
wire2_points <- get_points(input[[2]])

cross_points <- merge(wire1_points, wire2_points)[-1, ]
cross_points <- pmap(cross_points, ~c(.x,.y))

min(map_int(cross_points, ~ distance_to_cross(wire1_points, wire2_points, .))) - 2

