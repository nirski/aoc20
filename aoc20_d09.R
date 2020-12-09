library(tidyverse)
library(adventr)

# Sys.setenv("ADVENT_COOKIE" = read_lines("AOC_COOKIE"))

# Data ---------------------------------------------------------------------------------------------

d09_d0 <- read_advent_of_code(day = 9, cookie = read_lines("AOC_COOKIE")) %>% as.numeric() %>% na.omit()

# d09_ex <- c(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

# Part 1 -------------------------------------------------------------------------------------------

p <- 25
n <- length(d09_d0)
r <- (p+1):n

is_sum <- function(n, v, p) v[n] %in% (v[n - p:1] %>% combn(2) %>% colSums())
d09_r1 <- d09_d0[r][!map_lgl(r, is_sum, d09_d0, p)]

d09_r1

# Part 2 -------------------------------------------------------------------------------------------

for (i in 1:(n-1)) for (j in (i+1):n) if (sum(d09_d0[i:j]) == d09_r1) {x <- d09_d0[i:j]; break}

sum(range(x))
