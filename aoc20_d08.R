library(tidyverse)
library(adventr)

# Sys.setenv("ADVENT_COOKIE" = read_lines("AOC_COOKIE"))

# Data ---------------------------------------------------------------------------------------------

d08_d0 <- read_advent_of_code(day = 8, cookie = read_lines("AOC_COOKIE"))

d08_ex <- c("nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6")

d08_d1 <- enframe(d08_d0) %>%
    extract(value, c("instr", "value"), "^(acc|jmp|nop) (.*)$", convert = TRUE) %>%
    drop_na()

# Part 1 -------------------------------------------------------------------------------------------

d08_f1 <- function(ch) {
    instr <- d08_d2$instr
    value <- d08_d2$value
    visit <- rep(0, length(instr))
    accum <- 0
    i <- 1

    if (ch != 0) if (instr[ch] == "jmp") instr[ch] <- "nop" else if (instr[ch] == "nop") instr[ch] <- "jmp"
    
    while (all(visit < 2)) {
        visit[i] <- visit[i] + 1
        accum <- accum + ifelse(visit[i] < 2 && instr[i] == "acc", value[i], 0)
        i <- i + ifelse(instr[i] == "jmp", value[i], 1)
        if (i > length(instr)) break
    }
    
    list(terminated = all(visit < 2), accum = accum)
}

d08_f1(0)$accum

# Part 2 -------------------------------------------------------------------------------------------

d08_d1 %>%
    mutate(f = map(name, d08_f1)) %>%
    unnest_wider(f) %>%
    filter(terminated)
