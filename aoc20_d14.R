library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d14_d0 <- aoc_get_input(14, 2020, "AOC_COOKIE")
# d14_ex <- tibble(input = c(
#     "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X", "mem[8] = 11", "mem[7] = 101", "mem[8] = 0"
# ))
# d14_ex <- tibble(input = c(
#     "mask = 000000000000000000000000000000X1001X", "mem[42] = 100",
#     "mask = 00000000000000000000000000000000X0XX", "mem[26] = 1"
# ))

input <- d14_d0 %>% read_table("input") %>%
# input <- d14_ex %>%
    mutate(row = row_number(), mask = str_extract(input, "(?<=mask = ).+$")) %>%
    fill(mask) %>%
    filter(grepl("^mem", input)) %>%
    unglue::unglue_unnest(input, "mem[{address}] = {value}") %>%
    mutate(across(c(address, value), as.integer))

# Part 1 -------------------------------------------------------------------------------------------

i2b <- function(i) as.character(i %/% (2^(35:0)) %% 2) # convert integer to a char vector of length 36
b2i <- function(b) sum(as.integer(b) * (2^(35:0))) # inverse of i2b
m1 <- function(m) m %>% str_split("") %>% unlist() %>% str_replace("X", NA_character_)
v1 <- function(i, m) m %>% coalesce(i2b(i)) %>% b2i()

input %>%
    mutate(mask1 = map(mask, m1), value1 = map2_dbl(value, mask1, v1)) %>%
    group_by(address) %>%
    filter(row == max(row)) %>%
    with(sum(value1))

# Part 2 -------------------------------------------------------------------------------------------

rr <- function(s) if (is.na(s)) NA_character_ else if (s == "X") c("0", "1") else s
m2 <- function(m) {
    m %>%                                         # "X0X1"
        str_split("") %>%                         # list(c("X", "0", "X", "1"))
        pluck(1) %>%                              # c("X",  0, "X", "1")
        str_replace("0", NA_character_) %>%       # c("X",  NA, "X", "1")
        map(rr) %>%                               # list(c("0", "1"),  NA, c("0", "1"), "1")
        expand.grid(stringsAsFactors = FALSE) %>% # data.frame(Var1 = c("0", "1", "0", "1"), ...)
        asplit(1) %>%                             # split data.frame rowwise
        # transpose() %>% map(unlist) %>%         # split data.frame rowwise (alternative)
        map(as.character)
}
v2 <- function(i, m) map2_chr(i, m, v1)

waldo::compare(
    m2("X0X1"),
    list(c("0", NA, "0", "1"), c( "1", NA, "0", "1"), c("0", NA, "1", "1"), c("1", NA, "1", "1"))
)

input %>%
    mutate(mask2 = map(mask, m2), address2 = map2(address, mask2, v2)) %>%
    unnest(address2) %>%
    group_by(address2) %>%
    filter(row == max(row)) %>%
    with(sum(as.numeric(value)))
