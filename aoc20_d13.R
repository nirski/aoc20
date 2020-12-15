library(tidyverse)
library(aocodeR)
# library(bit64)

options(scipen = 999)

# Data ---------------------------------------------------------------------------------------------

d13_d0 <- aoc_get_input(13, 2020, "AOC_COOKIE")
# d13_ex <- c("939\n7,13,x,x,59,x,31,19")
# d13_ex <- c("939\n17,x,13,19")
# d13_ex <- c("939\n67,7,59,61")
# d13_ex <- c("939\n67,x,7,59,61")
# d13_ex <- c("939\n67,7,x,59,61")
# d13_ex <- c("939\n1789,37,47,1889")

input <- d13_d0 %>%
# input <- d13_ex %>%
    melt_csv() %>%
    pivot_wider(names_from = row, values_from = value) %>%
    transmute(now = `1`, id = `2`) %>%
    fill(now) %>%
    mutate(across(.fns = parse_integer))

# Part 1 -------------------------------------------------------------------------------------------

input %>%
    mutate(minutes = id - now %% id) %>%
    slice_min(minutes) %>%
    mutate(result = id * minutes)

# Part 2 -------------------------------------------------------------------------------------------

input %>%
    mutate(t = row_number() - 1L, a = id - t) %>%
    drop_na() %>%
    with(numbers::chinese(a, id)) # incorrect (but correct for all examples)

# Chinese Remainder Theorem explained --------------------------------------------------------------
input %>%
    mutate(t = row_number() - 1L, a = id - t) %>%
    drop_na() %>%
    mutate(
        p = prod(id) / id,
        i = map2_dbl(p, id, numbers::modinv),
        x = a * p * i
    ) %>%
    with(sum(x) %% prod(id))
