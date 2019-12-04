library(testthat)

source("day 3.R")

test_that("MD works", {
  expect_equal(manhattan_dist(c(3, 3), c(0, 0)), 6)
  expect_equal(manhattan_dist(c(0, 0), c(12, 6)), 18)
})

test_that("closest cross is working", {
  wireA1 <- c("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72")
  wireA2 <- c("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")

  wireB1 <- c("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51")
  wireB2 <- c("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7")

  expect_equal(closest_cross_distance(wireA1, wireA2), 159)
  expect_equal(closest_cross_distance(wireB1, wireB2), 135)
})

