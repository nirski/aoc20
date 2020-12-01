library(tidyverse)
library(readr)

# Data ---------------------------------------------------------------------------------------------

d01_d1 <- read_table("data/d01_input.txt", col_names = c("x"))

# Part 1 -------------------------------------------------------------------------------------------

d01_d1 %>%
    mutate(y = x) %>%
    expand(x, y) %>%
    filter(x < y) %>%
    mutate(sum = x + y) %>%
    filter(sum == 2020) %>%
    mutate(product = x * y)

# Part 2 -------------------------------------------------------------------------------------------

d01_d1 %>%
    mutate(y = x, z = x) %>%
    expand(x, y, z) %>%
    filter(x < y, y < z) %>%
    mutate(sum = x + y + z) %>%
    filter(sum == 2020) %>%
    mutate(product = x * y * z)
