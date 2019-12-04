library(testthat)
library(purrr)

# Part 1 ------------------------------------------------------------------

input <- 273025:767253

split_to_vec <- function(integer) {
  digits <- ceiling(log10(integer))
  vec_1 <- 10 ^ (digits:1)
  vec_2 <- vec_1 / 10
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


# Part 2 ------------------------------------------------------------------
possible_passwords <- map_lgl(input, possible_password)
reduced_input <- input[possible_passwords]

extra_rule <- function(input) {
  vector_input <- input %>% split_to_vec()

  true_double = FALSE
  group_size = 0
  for (i in 2:6) {
    if (vector_input[i] == vector_input[i - 1]) {
      if (group_size == 0) {
        group_size = 2
      } else {
        group_size <- group_size + 1
      }
    } else {
      if (group_size == 2) {
        true_double = TRUE
      }
      group_size = 0
    }
  }

  return(true_double | group_size == 2)
}

test_that("Extra rule works", {
  expect_true(extra_rule(112233))
  expect_false(extra_rule(123444))
  expect_true(extra_rule(111122))
})

sum(map_dbl(reduced_input, extra_rule))
