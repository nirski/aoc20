library(tidyverse)
library(aocodeR)
library(collections)

# Data ---------------------------------------------------------------------------------------------

d15_d0 <- aoc_get_input(15, 2020, "AOC_COOKIE")

input <- d15_d0 %>% str_split(",") %>% unlist() %>% as.integer()

# Functions ----------------------------------------------------------------------------------------

j1 <- function(v, s) {
    for (i in (length(v) + 1):s) {
        s <- which(v == last(v))
        n <- ifelse(length(s) == 1, 0, s[length(s)] - s[length(s) - 1])
        v <- c(v, n)
        # if (i %% 1e4 == 0) print(i)
    }
    last(v)
}

j2 <- function(v, s) {
    m <- dict()
    for (i in 1:(length(v)-1)) m$set(v[i], i)
    L <- last(v)
    for (i in (length(v)):(s - 1)) {
        n <- m$get(L, 0)
        m$set(L, i)
        L <- ifelse(n == 0, 0L, i - n)
        # if (i %% 1e4 == 0) print(i)
    }
    L
}

# Part 1 -------------------------------------------------------------------------------------------

# input %>% j1(2020)
input %>% j2(2020) # 620

# Part 2 -------------------------------------------------------------------------------------------

# input %>% j1(30000000) # too slow
input %>% j2(30000000) # 110871

# Heroes -------------------------------------------------------------------------------------------

# https://twitter.com/rundel/status/1338882215556558849
g2 <- function(x, end = 2020) {
    res = integer()
    for(i in 1:(length(x)-1)) res[ x[i] + 1 ] = i
    cur_val = x[length(x)]
    for (i in seq(length(x), end-1)) {
        if (is.na(res[cur_val + 1])) new_val = 0 else new_val = i - res[cur_val + 1]
        res[cur_val + 1] = i
        cur_val = new_val
    }
    cur_val
}

input %>% g2(30000000)

# https://twitter.com/drob/status/1338897063401181187
d2 <- function(x, s) {
    previous_indices <- rep(NA, s)
    previous_indices[head(x, -1) + 1] <- seq_len(length(x) - 1)
    last_value <- last(x)
    for (i in seq(length(x) + 1, s)) {
        previous_index <- previous_indices[last_value + 1L]
        if (is.na(previous_index)) new_value <- 0 else new_value <- i - 1 - previous_index
        previous_indices[last_value + 1] <- i - 1
        last_value <- new_value
    }
    last_value
}

input %>% d2(30000000)

# https://twitter.com/antoine_fabri/status/1338836033358721025
f2 <- function(v, s) {
    data <- v + 1
    last_pos <- integer(s)
    last_pos[head(data, -1)] <- seq_along(head(data, -1))
    last_num <- tail(data, 1)
    for (i in length(data):(s-1)) {
        last_pos_i <- last_pos[last_num]
        last_pos[last_num] <- i
        last_num <- if (last_pos_i) i - last_pos_i + 1 else 1
    }
    last_num - 1
}

input %>% f2(30000000)

# Benchmark ----------------------------------------------------------------------------------------

d12_b1 <- bench::mark(
    j1 = input %>% j1(1e5),
    j2 = input %>% j2(1e5),
    g2 = input %>% g2(1e5),
    d2 = input %>% d2(1e5),
    f2 = input %>% f2(1e5)
)

d12_b1
plot(d12_b1)
