library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Part 1 ------------------------------------------------------------------

input <- read_lines("data/day8") %>%
  as.integer()

split_to_layers <- function(string, width, height) {
  size <- width * height

  layer_list <- str_extract_all(string, paste0(".{", size, "}"), simplify = T) %>%
    as.list %>% map(~str_split(.x, "", simplify = T))

  return(layer_list)
}

split_to_vec <- function(integer) {
  digits <- ceiling(log10(integer))
  vec_1 <- 10 ^ (digits:1)
  vec_2 <- vec_1 / 10
  final_vec <- (integer %% vec_1) %/% vec_2

  return(final_vec)
}

layers <- input %>% split_to_layers(width = 25, height = 6)

zero_count <- layers %>% map_int(~sum(.x == "0"))

fewest_zero_layer <- layers[which(min(zero_count) == zero_count)] %>% flatten()

sum(fewest_zero_layer == "1") * sum(fewest_zero_layer == "2")


