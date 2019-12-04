library(testthat)
library(readr)
library(stringr)
library(dplyr)
library(purrr)

# Part 1 ------------------------------------------------------------------

run_intcode <- function(intcode){
  opcode <- 1

  while (intcode[opcode] != 99) {
    if (intcode[opcode] == 1) {
      # Addition
      intcode[intcode[opcode + 3] + 1] <-
        intcode[intcode[opcode + 1] + 1] +
        intcode[intcode[opcode + 2] + 1]
    } else if (intcode[opcode] == 2) {
      # Multiplication
      intcode[intcode[opcode + 3] + 1] <-
        intcode[intcode[opcode + 1] + 1] *
        intcode[intcode[opcode + 2] + 1]
    }
    # Advance
    opcode <- opcode + 4
  }

  return(intcode)
}

test_that("intcode works", {
  expect_equal(run_intcode(c(1,0,0,0,99)),
                c(2,0,0,0,99))
  expect_equal(run_intcode(c(2,3,0,3,99)),
               c(2,3,0,6,99))
  expect_equal(run_intcode(c(2,4,4,5,99,0)),
               c(2,4,4,5,99,9801))
  expect_equal(run_intcode(c(1,1,1,4,99,5,6,0,99)),
               c(30,1,1,4,2,5,6,0,99))
})

initial_input <- (read_lines("data/day2-1") %>%
  str_split(","))

initial_input <- as.integer(initial_input[[1]])

input <- initial_input
input[2] <- 12
input[3] <- 2

run_intcode(input)[1]


# Part 2 ------------------------------------------------------------------
modify_input <- function(noun, verb) {
  input <- initial_input
  input[2] <- noun
  input[3] <- verb

  return(input)
}

combination <- 0:99 %>%
list("noun" = ., "verb" = .) %>%
  cross(.filter = function(noun, verb) {
     modify_input(noun, verb)
    intcode <- modify_input(noun, verb) %>% run_intcode()
    return(19690720 != intcode[1])
  })


