library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d05_d1 <- aoc_get_input(5, 2020, "AOC_COOKIE") %>% read_table("input")

# Part 1 -------------------------------------------------------------------------------------------

d05_r1 <- d05_d1 %>%
    mutate(bin = input %>% str_replace_all(c("F" = "0", "B" = "1", "L" = "0", "R" = "1"))) %>%
    # separate(bin, c("row", "col"), 7) %>%
    # mutate(row = strtoi(row, 2), col = strtoi(col, 2), seat = row * 8 + col) %>%
    mutate(seat = bin %>% strtoi(2)) %>%
    arrange(seat)

d05_r1 %>% slice_max(seat)

# Part 2 -------------------------------------------------------------------------------------------

d05_r1 %>% slice_max(seat - lag(seat))
