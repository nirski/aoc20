library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d01_d1 <- aoc_get_input(1, 2020, "AOC_COOKIE") %>% melt_csv() %>% pull(value) %>% as.numeric()

# Part 1 -------------------------------------------------------------------------------------------

crossing(x = d01_d1, y = d01_d1) %>%
    mutate(sum = x + y, product = x * y) %>%
    filter(x < y, sum == 2020)

# Part 2 -------------------------------------------------------------------------------------------

crossing(x = d01_d1, y = d01_d1, z = d01_d1) %>%
    mutate(sum = x + y + z, product = x * y * z) %>%
    filter(x < y, y < z, sum == 2020)

# Benchmark ----------------------------------------------------------------------------------------

bench::mark(
    dplyr = crossing(x = d01_d1, y = d01_d1, z = d01_d1) %>%
        mutate(sum = x + y + z, product = x * y * z) %>%
        filter(x < y, y < z, sum == 2020) %>%
        pull(product),
    collapse = expand.grid(x = d01_d1, y = d01_d1, z = d01_d1) %>%
        collapse::ftransform(sum = x + y + z, product = x * y * z) %>%
        collapse::fsubset(x < y & y < z & sum == 2020) %>%
        collapse::get_vars("product") %>% as.numeric(),
    base = {
        x <- d01_d1
        for (i in x) for (j in x) for (k in x) if (i+j+k == 2020) {z <- i*j*k; break}
        z
    },
    # https://twitter.com/drob/status/1333650983726034946
    drob1 = prod(d01_d1[d01_d1 %in% (2020 - outer(d01_d1, d01_d1, "+"))]),
    drob2 = prod(intersect(d01_d1, 2020 - outer(d01_d1, d01_d1, "+")))
)
