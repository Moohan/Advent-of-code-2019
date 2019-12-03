library(dplyr)
library(testthat)
library(readr)

# Part 1 ------------------------------------------------------------------
fuel_required <- function(mass){
  fuel <- floor(mass / 3) - 2 
  
  return(fuel)
}

test_that("fuel required test cases", {
  expect_equal(fuel_required(12), 2)
  expect_equal(fuel_required(14), 2)
  expect_equal(fuel_required(1969), 654)
  expect_equal(fuel_required(100756), 33583)
})

input <- read_csv("data/day1-1", col_names = FALSE)

sum(fuel_required(input$X1))


# Part 2 ------------------------------------------------------------------

fuel_required_adjusted <- Vectorize(function(mass) {
  fuel <- fuel_required(mass) 
  
  total <- 0
  
  while (fuel > 0) {
    total <- total + fuel 
    fuel <- fuel_required(fuel)
  }
  
  return(total)
})

test_that("fuel required test cases", {
  expect_equal(fuel_required_adjusted(14), 2)
  expect_equal(fuel_required_adjusted(1969), 966)
  expect_equal(fuel_required_adjusted(100756), 50346)
})

sum(fuel_required_adjusted(input$X1))

