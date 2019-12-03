library(dplyr)
library(testthat)
library(readr)

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



