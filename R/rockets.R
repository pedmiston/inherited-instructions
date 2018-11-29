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

MedianRTs <- Rockets %>%
  group_by(workerId) %>%
  summarize(
    median_rt = median(rt, na.rm = TRUE)
  )

rt_plot <- ggplot(MedianRTs) +
  aes(fct_reorder(workerId, median_rt), median_rt) +
  geom_point() +
  scale_x_discrete(labels = NULL) +
  geom_hline(yintercept = 500)

workers_who_didnt_respond_too_fast <- MedianRTs %>%
  filter(median_rt > 500) %>%
  .$workerId

workers_who_completed_all_trials <- count(Rockets, workerId) %>%
  mutate(is_valid = as.numeric(n == 128)) %>%
  filter(is_valid == 1) %>%
  .$workerId

Rockets <- Rockets %>%
  filter(workerId %in% workers_who_completed_all_trials,
         workerId %in% workers_who_didnt_respond_too_fast)

accuracy_plot <- ggplot(Rockets) +
  aes(block_ix, is_correct, color = generation_f) +
  geom_line(aes(group = workerId), stat = "summary", fun.y = "mean") +
  geom_smooth(aes(group = generation_f), method = "lm", se = FALSE) +
  geom_vline(xintercept=7, linetype = 2)
