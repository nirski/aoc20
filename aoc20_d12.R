library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d12_d0 <- aoc_get_input(12, 2020, "AOC_COOKIE")

input <- d12_d0 %>% str_split("\n") %>% unlist()
# input <- c("F10", "N3", "F7", "R90", "F11")

fa <- c(N = 1i, E = 1, S = -1i, W = -1)
ft <- c(L = 1, R = -1)

# Part 1 -------------------------------------------------------------------------------------------

posn <- 0
face <- fa["E"]

for (i in input) {
    f <- str_sub(i, 1, 1)
    n <- str_sub(i, 2) %>% as.integer()
    if (f == "F") posn <- posn + n * face
    else if (f %in% names(ft)) face <- face * 1i^(ft[f] * n / 90)
    else posn <- posn + n * fa[f]
}

posn
abs(Re(posn)) + abs(Im(posn))

# Part 2 -------------------------------------------------------------------------------------------

posn <- 0
wayp <- 10+1i

for (i in input) {
    f <- str_sub(i, 1, 1)
    n <- str_sub(i, 2) %>% as.integer()
    if (f == "F") posn <- posn + n * wayp
    else if (f %in% names(ft)) wayp <- wayp * 1i^(ft[f] * n / 90)
    else wayp <- wayp + n * fa[f]
}

posn
abs(Re(posn)) + abs(Im(posn))
