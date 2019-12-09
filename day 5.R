library(testthat)
library(readr)
library(stringr)
library(dplyr)

read_opcode <- function(intcode, pointer) {
  opcode <- intcode[pointer]

  return(opcode %% 100)
}

get_parameter <- function(intcode, pointer, parameter) {
  parameters <- intcode[pointer] %/% 100 %>%
    as.character() %>%
    str_pad(width = 3, side = "left", pad = "0")

  return(as.integer(str_sub(parameters, 4 - parameter, 4 - parameter)))
}

position_mode <- function(intcode, pointer, parameter) {
  return(get_parameter(intcode, pointer, parameter) == 0)
}

do_operation <- function(intcode, pointer, operation) {
  first_param <- if_else(position_mode(intcode, pointer, 1),
                         intcode[abs(intcode[pointer + 1]) + 1],
                         intcode[pointer + 1])
  second_param <- if_else(position_mode(intcode, pointer, 2),
                          intcode[abs(intcode[pointer + 2]) + 1],
                          intcode[pointer + 2])


  if (position_mode(intcode, pointer, 3)) {
    intcode[intcode[pointer + 3] + 1] <-
      operation(
        first_param,
        second_param
      )
  } else {
    intcode[pointer + 3] <-
      operation(
        first_param,
        second_param
      )
  }
  return(intcode)
}

do_jump <- function(intcode, pointer, jump_if) {
  first_param <- if_else(position_mode(intcode, pointer, 1),
                         intcode[abs(intcode[pointer + 1]) + 1],
                         intcode[pointer + 1])
  second_param <- if_else(position_mode(intcode, pointer, 2),
                          intcode[abs(intcode[pointer + 2]) + 1],
                          intcode[pointer + 2])


  if (xor(first_param == 0,
          jump_if)) {
    pointer <- second_param
  } else {
    pointer <- pointer + 3
  }

  return(pointer)
}

do_comparison <- function(intcode, pointer, comparison) {
  first_param <- if_else(position_mode(intcode, pointer, 1),
                         intcode[abs(intcode[pointer + 1]) + 1],
                         intcode[pointer + 1])
  second_param <- if_else(position_mode(intcode, pointer, 2),
                          intcode[abs(intcode[pointer + 2]) + 1],
                          intcode[pointer + 2])
  to_store <- if_else(comparison(first_param, second_param),
                      1,
                      0)

  if (position_mode(intcode, pointer, 3)) {
    intcode[intcode[pointer + 3] + 1] <- to_store
  } else {
    intcode[pointer + 3] <- to_store
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
    } else if (opcode == 5) {
      pointer <- do_jump(intcode, pointer, jump_if = TRUE)
    } else if (opcode == 6) {
      pointer <- do_jump(intcode, pointer, jump_if = FALSE)
    } else if (opcode == 7) {
      intcode <- do_comparison(intcode, pointer, `<`)
      pointer <- pointer + 4
    } else if (opcode == 8) {
      intcode <- do_comparison(intcode, pointer, `==`)
      pointer <- pointer + 4
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
  # Input = 999
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

test_that("comparisons work", {
  # Input = 8
  expect_message(run_intcode(c(3,9,8,9,10,9,4,9,99,-1,8)), "Output is: 1")
  # Input != 8
  expect_message(run_intcode(c(3,9,8,9,10,9,4,9,99,-1,8)), "Output is: 0")
  # Input is 0
  expect_message(run_intcode(c(3,9,7,9,10,9,4,9,99,-1,8)), "Output is: 1")
  expect_message(run_intcode(c(3,3,1108,-1,8,3,4,3,99)), "Output is: 0")
  expect_message(run_intcode(c(3,3,1107,-1,8,3,4,3,99)), "Output is: 1")
})

test_that("jump works", {
  expect_message(run_intcode(c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)), "Output is: 0")

})

test_that("big part 2 example", {
  # Input 7
  expect_message(run_intcode(c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)), "Output is: 999")
  # Input 8
  expect_message(run_intcode(c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)), "Output is: 1000")
  # Input 9
  expect_message(run_intcode(c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)), "Output is: 1001")
})

initial_input <-
  read_lines("data/day5") %>%
  str_split(",") %>%
  unlist() %>%
  as.integer()

run_intcode(initial_input)
