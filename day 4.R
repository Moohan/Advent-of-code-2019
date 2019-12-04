library(testthat)
library(purrr)

input <- 273025:767253

split_to_vec <- function(integer) {
  digits <- ceiling(log10(integer))
  vec_1 <- 10^(digits:1)
  vec_2 <- vec_1/10
  final_vec <- (integer %% vec_1) %/% vec_2

  return(final_vec)
}

possible_password <- function(input) {
  vector_input <- input %>% split_to_vec()

  possible = TRUE
  double = FALSE
  x = 1
  while (x < 6) {
    if (vector_input[x] > vector_input[x + 1]) {
      possible = FALSE
    }

    if (vector_input[x] == vector_input[x + 1]) {
      double = TRUE
    }
    x <- x + 1
  }

  if (double == FALSE) {
    possible = FALSE
  }

  return(possible)
}

test_that("password selection works", {
  expect_true(possible_password(111111))
  expect_false(possible_password(223450))
  expect_false(possible_password(123789))
})

sum(map_lgl(input, possible_password))
