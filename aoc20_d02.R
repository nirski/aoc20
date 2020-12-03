library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d02_d1 <- aoc_get_input(2, 2020, "AOC_COOKIE") %>%
    read_table("input") %>%
    extract(input, c("from", "to", "char", "string"), "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)") %>%
    # https://twitter.com/Ben_Guinaudeau/status/1334186253064019971
    # unglue::unglue_unnest(input, "{from}-{to} {char}: {string}")
    mutate(across(from:to, as.integer))

# Part 1 -------------------------------------------------------------------------------------------

d02_f1 <- function(f, t, c, s) {
    str_count(s, c) %>% between(f, t)
}

d02_d1 %>%
    mutate(valid = pmap_lgl(list(from, to, char, string), d02_f1)) %>%
    tally(valid)

# Part 2 -------------------------------------------------------------------------------------------

d02_f2 <- function(f, t, c, s) {
    first <- str_sub(s, f, f) == c
    second <- str_sub(s, t, t) == c
    xor(first, second)
}

d02_d1 %>%
    mutate(valid = pmap_lgl(list(from, to, char, string), d02_f2)) %>%
    tally(valid)
