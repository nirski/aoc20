library(tidyverse)
library(adventr)
# https://stackoverflow.com/a/9460241
library(expm)

options(scipen = 999)

# Sys.setenv("ADVENT_COOKIE" = read_lines("AOC_COOKIE"))

# Data ---------------------------------------------------------------------------------------------

d10_d0 <- read_advent_of_code(day = 10, cookie = read_lines("AOC_COOKIE"))

# d10_ex <- c(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

input <- d10_d0 %>% as.numeric() %>% na.omit() %>% append(0, 0) %>% append(max(.) + 3)

# Part 1 -------------------------------------------------------------------------------------------

input %>% sort() %>% diff() %>% table() %>% prod()

# Part 2 -------------------------------------------------------------------------------------------

difs <- outer(input, input, `-`)

adjm <- (difs >= 1 & difs <= 3) * 1

# https://en.wikipedia.org/wiki/Adjacency_matrix#Matrix_powers
# G^n[i,j] = number of paths of length n from vertex i to vertex j
# G^0 + G^1 + A^2 + A^3 + ... = (I - G)^(-1)
# https://cs.stackexchange.com/a/93928
# https://en.wikipedia.org/wiki/Band_matrix
solve(diag(length(input)) - adjm)[length(input), 1]

# Benchmark ----------------------------------------------------------------------------------------

d10_b1 <- bench::mark(
    expm = seq_along(input) %>%
        map(function(p) t(adjm) %^% p) %>%
        reduce(`+`) %>%
        .[1, length(input)],
    matrixcalc = seq_along(input) %>%
        map(function(p) matrix.power(t(adjm), p)) %>%
        reduce(`+`) %>%
        .[1, length(input)],
    solve = solve(diag(length(input)) - adjm)[length(input), 1]
)

d10_b1
plot(d10_b1)
