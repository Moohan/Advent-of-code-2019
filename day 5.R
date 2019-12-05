library(testthat)
library(readr)
library(stringr)
library(dplyr)

read_opcode <- function(intcode, pointer) {
  opcode <- intcode[pointer]

  return(opcode %% 100)
}

position_mode <- function(intcode, pointer, parameter) {
  parameters <- intcode[pointer] %/% 100 %>%
    as.character() %>%
    str_pad(width = 3, side = "left", pad = "0")

  return(as.integer(str_sub(parameters, 4 - parameter, 4 - parameter)) == 0)
}

do_operation <- function(intcode, pointer, operation) {
  if (position_mode(intcode, pointer, 3)) {
    intcode[intcode[pointer + 3] + 1] <-
      operation(
        if_else(position_mode(intcode, pointer, 1),
          intcode[abs(intcode[pointer + 1]) + 1],
          intcode[pointer + 1]
        ),
        if_else(position_mode(intcode, pointer, 2),
          intcode[abs(intcode[pointer + 2]) + 1],
          intcode[pointer + 2]
        )
      )
  } else {
    intcode[pointer + 3] <-
      operation(
        if_else(position_mode(intcode, pointer, 1),
          intcode[abs(intcode[pointer + 1]) + 1],
          intcode[pointer + 1]
        ),
        if_else(position_mode(intcode, pointer, 2),
          intcode[abs(intcode[pointer + 2]) + 1],
          intcode[pointer + 2]
        )
      )
  }
  return(intcode)
}

run_intcode <- function(intcode) {
  pointer <- 1

  while (read_opcode(intcode, pointer) != 99) {
    opcode <- read_opcode(intcode, pointer)
    print(paste(
      "Pointer is at position", pointer,
      "value", intcode[pointer],
      "opcode", opcode
    ))


    if (opcode == 1) {
      # Addition
      intcode <- do_operation(intcode, pointer, `+`)
      # Advance
      pointer <- pointer + 4
    } else if (opcode == 2) {
      # Multiplication
      intcode <- do_operation(intcode, pointer, `*`)
      # Advance
      pointer <- pointer + 4
    } else if (opcode == 3) {
      # Take input
      intcode[intcode[pointer + 1] + 1] <-
        as.integer(rstudioapi::showPrompt("Input", "Input:"))
      # Advance
      pointer <- pointer + 2
    } else if (opcode == 4) {
      # Ouput
      message(paste(
        "Output is:",
        as.character(intcode[intcode[pointer + 1] + 1])
      ))
      # Advance
      pointer <- pointer + 2
    }
  }
  message("Program Halted")
  return(intcode)
}


test_that("old intcode works", {
  expect_equal(
    run_intcode(c(1, 0, 0, 0, 99)),
    c(2, 0, 0, 0, 99)
  )
  expect_equal(
    run_intcode(c(2, 3, 0, 3, 99)),
    c(2, 3, 0, 6, 99)
  )
  expect_equal(
    run_intcode(c(2, 4, 4, 5, 99, 0)),
    c(2, 4, 4, 5, 99, 9801)
  )
  expect_equal(
    run_intcode(c(1, 1, 1, 4, 99, 5, 6, 0, 99)),
    c(30, 1, 1, 4, 2, 5, 6, 0, 99)
  )
})

test_that("new opcodes work", {
  expect_message(
    run_intcode(c(3, 0, 4, 0, 99)),
    "Output is: 999"
  )
})

test_that("positional mode works", {
  expect_equal(run_intcode(c(1002,4,3,4,33)),
               c(1002, 4,3,4,99))
  expect_equal(run_intcode(c(1101,100,-1,4,0)),
               c(1101,100,-1,4,99))
})

initial_input <-
  read_lines("data/day5") %>%
  str_split(",") %>%
  unlist() %>%
  as.integer()

run_intcode(initial_input)
