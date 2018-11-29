# ---- rockets
library(tidyverse)
library(rockets)

data("Rockets")

recode_generation <- function(frame) {
  map <- data_frame(
    generation = 1:2,
    generation_c = c(-0.5, 0.5),
    generation_f = factor(c("Generation 1", "Generation 2"))
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}

Rockets <- Rockets %>%
  recode_generation()

ValidSubjects <- count(Rockets, workerId) %>%
  mutate(is_valid = as.numeric(n == 128)) %>%
  select(workerId, is_valid)

Rockets <- left_join(Rockets, ValidSubjects)

accuracy_plot <- ggplot(filter(Rockets, is_valid == 1)) +
  aes(block_ix, is_correct, color = generation_f) +
  geom_line(aes(group = workerId), stat = "summary", fun.y = "mean") +
  geom_smooth(aes(group = generation_f), method = "lm", se = FALSE)
