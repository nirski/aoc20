library(tidyverse)
# remotes::install_github("benjaminguinaudeau/adventr")
library(adventr)

# Sys.setenv("ADVENT_COOKIE" = read_lines("AOC_COOKIE"))

# Data ---------------------------------------------------------------------------------------------

d07_d1 <- read_advent_of_code(day = 7, cookie = read_lines("AOC_COOKIE"))

d07_d2 <- enframe(d07_d1)

d07_r1 <- d07_d2 %>%
    extract(value, c("outside", "inside"), "^(.*) bags contain (.*)$") %>%
    separate_rows(inside, sep = ", ") %>%
    extract(inside, c("n", "inside"), "^([0-9]+) (.*) bags*\\.*$", convert = TRUE) %>%
    mutate(
        level1 = ifelse(inside == "shiny gold", 1, NA),
        n2 = ifelse(outside == "shiny gold", n, NA),
        level2 = ifelse(outside == "shiny gold", 1, NA)
    ) %>%
    filter(!is.na(inside))

# Part 1 -------------------------------------------------------------------------------------------

d07_f1 <- function(df, lev) {
    out <- df %>% filter(level1 == lev) %>% pull(outside)
    df %>% mutate(level1 = ifelse(inside %in% out, lev + 1, level1))
}

accumulate(1:9, d07_f1, .init = d07_r1) %>%
    map(. %>% filter(!is.na(level1)) %>% distinct(outside) %>% count()) %>%
    unlist()

# Part 2 -------------------------------------------------------------------------------------------

d07_f2 <- function(df, lev) {
    ins <- df %>% filter(level2 == lev) %>% pull(inside)
    df2 <- df %>% filter(level2 == lev) %>% transmute(outside = inside, n2)
    df %>%
        left_join(df2, by = "outside") %>%
        mutate(
            level2 = ifelse(outside %in% ins, lev + 1, level2),
            n2 = coalesce(n2.x, 0) + coalesce(n2.y * n, 0)
        ) %>%
        select(-n2.x, -n2.y)
}

accumulate(1:9, d07_f2, .init = d07_r1) %>%
    map(. %>% filter(!is.na(level2)) %>% tally(n2)) %>%
    unlist()
