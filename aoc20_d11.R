library(tidyverse)
library(adventr)

# Sys.setenv("ADVENT_COOKIE" = read_lines("AOC_COOKIE"))

# Data ---------------------------------------------------------------------------------------------

d11_d0 <- read_advent_of_code(day = 11, cookie = read_lines("AOC_COOKIE"))

d11_ex <- c(
    "L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL", "L.LL.LL.LL",
    "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL", "L.LLLLLL.L", "L.LLLLL.LL"
)

input <- d11_d0 %>% .[-99] %>% map(strsplit, "") %>% unlist() %>% matrix(98, byrow = TRUE)
# input <- d11_ex %>% map(strsplit, "") %>% unlist() %>% matrix(10, byrow = TRUE)

mborder <- function(m, b) {
    nr <- rep(b, ncol(m))
    nc <- rep(b, nrow(m) + 2)
    cbind(nc, rbind(nr, m, nr), nc) %>% `dimnames<-`(NULL)
}

# Part 1 -------------------------------------------------------------------------------------------

step_life1 <- function(m, dummy) {
    n <- m
    for (i in 2:(ncol(m)-1)) for (j in 2:(nrow(m)-1)) {
        s <- sum(m[(i-1):(i+1), (j-1):(j+1)] == "#")
        if (m[i, j] == "L" && s == 0) n[i, j] <- "#"
        if (m[i, j] == "#" && s >= 5) n[i, j] <- "L"
    }
    if (all(n == m)) done(n) else n # identical(n, m)
}

1:100 %>% reduce(step_life1, .init = input %>% mborder(".")) %>% {. == "#"} %>% sum() # 2468 # 37

# Part 2 -------------------------------------------------------------------------------------------

rview <- function(m, r, c, dr, dc) { # rview(m, 2, 8, 1, -1) = first seen left-down from m[2, 8]
    if (dr == 0 & dc == 0) return(m[r, c])
    lr <- ifelse(dr == 1, nrow(m) - r, ifelse(dr == -1, r - 1, Inf))
    lc <- ifelse(dc == 1, ncol(m) - c, ifelse(dc == -1, c - 1, Inf))
    for (i in 1:min(lr, lc)) {
        r <- r + dr
        c <- c + dc
        if (m[r, c] == "#") return("#")
        if (m[r, c] == "L") return("L")
    }
    return("L")
}

step_life2 <- function(m, dummy) {
    n <- m
    for (r in 2:(nrow(m)-1)) for (c in 2:(ncol(m)-1)) {
        s <- 0
        for (dr in (-1):1) for (dc in (-1):1) s <- s + (rview(m, r, c, dr, dc) == "#")
        if (m[r, c] == "L" && s == 0) n[r, c] <- "#"
        if (m[r, c] == "#" && s >= 6) n[r, c] <- "L"
    }
    if (all(n == m)) done(n) else n # identical(n, m)
}

1:100 %>% reduce(step_life2, .init = input %>% mborder("L")) %>% {. == "#"} %>% sum() # 2214 # 26
