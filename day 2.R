library(testthat)
library(readr)
library(stringr)

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

input <- (read_lines("data/day2-1") %>%
  str_split(","))

input <- as.integer(input[[1]])

input[2] <- 12
input[3] <- 2

run_intcode(input)[1]


  