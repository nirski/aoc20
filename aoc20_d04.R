library(tidyverse)
library(aocodeR)

# Data ---------------------------------------------------------------------------------------------

d04_d1 <- aoc_get_input(4, 2020, "AOC_COOKIE") %>% melt_csv()

# Part 1 -------------------------------------------------------------------------------------------

d04_r1 <- d04_d1 %>%
    separate_rows(value, sep = " ") %>%
    mutate(id = cumsum(data_type == "missing") + 1) %>%
    drop_na() %>%
    separate(value, c("key", "value"), ":") %>%
    select(key:id) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    select(-cid) %>%
    drop_na() %>%
    mutate(valid1 = TRUE)

d04_r1 %>% count(valid1)

# Part 2 -------------------------------------------------------------------------------------------

d04_r2 <- d04_d2 %>%
    mutate(
        # byr (Birth Year) - four digits; at least 1920 and at most 2002.
        byr2 = between(as.integer(byr), 1920, 2002),
        # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        iyr2 = between(as.integer(iyr), 2010, 2020),
        # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        eyr2 = between(as.integer(eyr), 2020, 2030),
        # hgt (Height) - a number followed by either cm or in:
        # - If cm, the number must be at least 150 and at most 193.
        # - If in, the number must be at least 59 and at most 76.
        hgt2 = case_when(
            grepl("cm$", hgt) & between(parse_number(hgt), 150, 193) ~ TRUE,
            grepl("in$", hgt) & between(parse_number(hgt), 59, 76) ~ TRUE,
            TRUE ~ FALSE
        ),
        # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        hcl2 = grepl("^#[0-9a-f]{6}$", hcl),
        # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        ecl2 = grepl("^amb|blu|brn|gry|grn|hzl|oth$", ecl),
        # pid (Passport ID) - a nine-digit number, including leading zeroes.
        pid2 = grepl("^[0-9]{9}$", pid),
        # cid (Country ID) - ignored, missing or not.
        valid2 = byr2 & iyr2 & eyr2 & hgt2 & hcl2 & ecl2 & pid2
    )

d04_r2 %>% count(valid2)

# Validate -----------------------------------------------------------------------------------------

library(validate)

v <- validator(.file = "aoc20_d04.yaml")

meta(v)

d04_dv <- d04_d2 %>%
    mutate(across(ends_with("yr"), as.integer)) %>%
    extract(hgt, c("hgt_value", "hgt_unit"), "^(\\d+)([cm|in]*)$", remove = FALSE) %>%
    mutate(hgt_value = as.integer(hgt_value))

d04_cf <- confront(d04_dv, v)

d04_cf
summary(d04_cf)
errors(d04_cf)
barplot(d04_cf, main = "AoC")

d04_cf %>% aggregate("record") %>% as_tibble() %>% tally(rel.pass == 1)

# Pointblank ---------------------------------------------------------------------------------------

library(pointblank)

d04_d2 %>%
    mutate(across(ends_with("yr"), as.integer)) %>%
    extract(hgt, c("hgt_value", "hgt_unit"), "^(\\d+)([cm|in]*)$", remove = FALSE) %>%
    mutate(hgt_value = as.integer(hgt_value)) %>%
    mutate(hgt_cm = ifelse(hgt_unit == "in", hgt_value * 2.54, hgt_value)) %>%
    create_agent() %>% 
    col_vals_between(vars(byr), 1920, 2002) %>% 
    col_vals_between(vars(iyr), 2010, 2020) %>% 
    col_vals_between(vars(eyr), 2020, 2030) %>% 
    col_vals_regex(vars(hcl), "^#[0-9a-f]{6}$") %>% 
    col_vals_in_set(vars(ecl), c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
    col_vals_regex(vars(pid), "^[0-9]{9}$") %>% 
    col_vals_between(vars(hgt_cm), 150, 193) %>% 
    interrogate() -> valid_report

valid_report
valid_report %>% get_sundered_data(type = "pass")
