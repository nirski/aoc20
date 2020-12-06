library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d06_d1 <- aoc_get_input(6, 2020, "AOC_COOKIE")

d06_d2 <- d06_d1 %>%
    melt_csv() %>%
    mutate(group_id = cumsum(data_type == "missing") + 1) %>%
    filter(data_type != "missing") %>%
    add_count(group_id, name = "group_size") %>%
    separate_rows(value, sep = "") %>%
    filter(value != "")

# Part 1 -------------------------------------------------------------------------------------------

d06_d2 %>%
    count(group_id, value) %>%
    count(group_id) %>%
    tally(n)

# Part 2 -------------------------------------------------------------------------------------------

d06_d2 %>%
    count(group_id, group_size, value) %>%
    filter(n == group_size) %>%
    count()
