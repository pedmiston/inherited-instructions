# ---- gems
library(tidyverse)
library(lme4)
library(lattice)

trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
trellis.par.set("fontsize", list(text=14))

library(gems)
data("Gems")
data("SimpleHill")
data("Survey")
data("Instructions")
data("InstructionsCoded")
data("Bots")

t_ <- get_theme()

TestLandscapeCurrentScores <- SimpleHill %>%
  transmute(current_x = x, current_y = y, current_score = score)
TestLandscapeGemScores <- SimpleHill %>%
  transmute(gem_x = x, gem_y = y, gem_score = score)

valid_subjs <- Gems %>%
  count(subj_id) %>%
  mutate(is_valid = n == 160) %>%
  filter(is_valid == 1) %>%
  .$subj_id

Gems <- Gems %>%
  filter(version == 1.3, subj_id %in% valid_subjs) %>%
  mutate_distance_2d() %>%
  left_join(TestLandscapeCurrentScores) %>%
  melt_trial_stims() %>%
  left_join(TestLandscapeGemScores) %>%
  rank_stims_in_trial() %>%
  filter(selected == gem_pos) %>%
  recode_generation() %>%
  recode_block() %>%
  recode_trial_poly()

Bots <- Bots %>%
  filter(trial < 80) %>%
  mutate_distance_2d() %>%
  left_join(TestLandscapeCurrentScores) %>%
  melt_trial_stims() %>%
  left_join(TestLandscapeGemScores) %>%
  rank_stims_in_trial() %>%
  filter(selected == gem_pos)

BotsMirror <- bind_rows(
  `1` = Bots,
  `2` = Bots,
  .id = "block_ix_chr"
) %>%
  mutate(block_ix = as.integer(block_ix_chr)) %>%
  select(-block_ix_chr)

GemsFinal <- Gems %>%
  group_by(generation, subj_id, block_ix) %>%
  filter(trial == max(trial)) %>%
  ungroup()

BotsFinal <- Bots %>%
  filter(trial == 79) %>%
  group_by(simulation_type, block_ix) %>%
  summarize(score = mean(score)) %>%
  ungroup()

InstructionsSummarized <- InstructionsCoded %>%
  group_by(subj_id) %>%
  summarize(instructions_score = sum(score)) %>%
  recode_instructions_score()

GemsCoded <- left_join(Gems, InstructionsSummarized) %>%
  drop_na(instructions_score)

GemsFinalCoded <- left_join(GemsFinal, InstructionsSummarized) %>%
  drop_na(instructions_score)

InheritanceMap <- select(Gems, subj_id, generation, inherit_from) %>% unique()
InheritanceMap <- left_join(InheritanceMap, InstructionsSummarized)

Gen1 <- InheritanceMap %>%
  filter(generation == 1) %>%
  select(-inherit_from, -generation) %>%
  rename(inherit_from = subj_id)

Gen2 <- InheritanceMap %>%
  filter(generation == 2) %>%
  select(-instructions_score, -instructions_label)

InheritanceMap <- left_join(Gen1, Gen2) %>%
  recode_instructions_score() %>%
  drop_na() %>%
  filter(inherit_from %in% valid_subjs, subj_id %in% valid_subjs)

Gen2Block1 <- Gems %>%
  filter(generation == 2, block_ix == 1) %>%
  left_join(InheritanceMap) %>%
  drop_na(instructions_score)

Gen2Block2 <- Gems %>%
  filter(generation == 2, block_ix == 2) %>%
  left_join(InheritanceMap) %>%
  drop_na(instructions_score)

Gen1Block1 <- Gems %>%
  filter(generation == 1, block_ix == 1) %>%
  filter(subj_id %in% InheritanceMap$inherit_from)

Gen1Block2 <- Gems %>%
  filter(generation == 1, block_ix == 2) %>%
  filter(subj_id %in% InheritanceMap$subj_id)

subj_map <- select(Gems, subj_id, version, generation) %>% unique() %>% drop_na()
inheritance_map <- select(Gems, subj_id, version, generation, inherit_from) %>% unique() %>% drop_na()

Survey <- Survey %>%
  left_join(subj_map) %>%
  filter(version %in% c(1.3, 1.4)) %>%
  recode_generation()

Instructions <- left_join(Instructions, subj_map) %>%
  select(subj_id, version, generation, instructions) %>%
  filter(version %in% c(1.3, 1.4))

# methods ----
simple_hill_landscape <- make_landscape(SimpleHill, "Simple hill")

path_data <- data_frame(
  x = c(0, 4, 9, 11, 8, 16, 21, 18),
  xend = c(4, 9, 11, 8, 16, 21, 18, 19),
  y = c(0, 1, 5, 10, 15, 16, 21, 26),
  yend = c(1, 5, 10, 15, 16, 21, 26, 31)
)

gem_data <- data_frame(
  x = c(7, 8, 14, 6, 17, 10) + 9,
  y = c(4, 6, 12, 13, 8, 17) + 21
)

radius_data <- data_frame(
  x0 = 19,
  y0 = 31,
  r = 8
)

trial_plot <- ggplot() +
  geom_point(aes(x, y), data = gem_data) +
  geom_segment(aes(x, y, xend = xend, yend = yend), data = path_data) +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r), data = radius_data) +
  annotate("point", x = 50, y = 50, shape = 4, size = 4) +
  geom_vline(xintercept = 50, linetype = 2, color = "gray") +
  geom_hline(yintercept = 50, linetype = 2, color = "gray") +
  coord_fixed(xlim = c(0, 70), ylim = c(0, 70), expand = FALSE) +
  scale_x_continuous("orientation", breaks = seq(0, 70, by = 10)) +
  scale_y_continuous("bar width", breaks = seq(0, 70, by = 10))

# overall ----
block1_mod <- lmer(score ~ generation_c * (trial_z + trial_z_sqr + trial_z_cub) + 
                     (trial_z + trial_z_sqr + trial_z_cub|subj_id),
                   data = filter(Gems, block_ix == 1))

block1_preds <- expand.grid(generation = c(1, 2), trial = 1:78, block_ix = 1) %>%
  recode_trial_poly() %>%
  recode_generation() %>%
  cbind(., AICcmodavg::predictSE(block1_mod, newdata = ., se = TRUE)) %>%
  rename(score = fit, se = se.fit)

block2_mod <- lmer(score ~ generation_c * (trial_z + trial_z_sqr + trial_z_cub) + (trial_z + trial_z_sqr + trial_z_cub|subj_id),
                   data = filter(Gems, block_ix == 2))

block2_preds <- expand.grid(generation = c(1, 2), trial = 1:78, block_ix = 2) %>%
  recode_trial_poly() %>%
  recode_generation() %>%
  cbind(., AICcmodavg::predictSE(block2_mod, newdata = ., se = TRUE)) %>%
  rename(score = fit, se = se.fit)

score_by_block_plot <- ggplot(Gems) +
  aes(trial, score) +
  geom_line(aes(group = generation_f, color = generation_f), stat = "summary", fun.y = "mean",
            size = 1.4) +
  geom_smooth(aes(ymin = score - se, ymax = score + se, group = generation_f, fill = generation_f),
              data = block1_preds, stat = "identity", color = NA, show.legend = FALSE) +
  geom_smooth(aes(ymin = score - se, ymax = score + se, group = generation_f, fill = generation_f),
              data = block2_preds, stat = "identity", color = NA, show.legend = FALSE) +
  geom_line(aes(group = simulation_type), color = "gray", linetype = "longdash", data = BotsMirror,
            stat = "summary", fun.y = "mean", size = 1.4) +
  facet_wrap("block_ix") +
  scale_color_manual("Generation", values = t_$get_colors("blue", "green")) +
  scale_fill_manual("Generation", values = t_$get_colors("blue", "green")) +
  theme(legend.position = "top")

# instructions ----
instructions_coded_plot <- ggplot(InstructionsCoded) +
  aes(factor(dimension)) +
  geom_bar(aes(fill = factor(score)), stat = "count", position = "stack") +
  scale_fill_discrete("", labels = c("Didn't mention", "Incomplete or inaccurate", "Correct")) +
  xlab("")

instructions_summarized_plot <- ggplot(InstructionsSummarized) +
  aes(instructions_label, y = ..count..) +
  geom_bar(aes(fill = instructions_label)) +
  scale_x_discrete("") +
  scale_y_continuous("") +
  coord_flip() +
  theme(legend.position = "none")

instruction_quality_and_performance_plot <- ggplot(GemsCoded) +
  aes(trial, score, color = instructions_label) +
  geom_line(stat = "summary", fun.y = "mean") +
  xlab("") +
  scale_color_discrete("") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.75, 0.33))

instruction_quality_and_final_performance_plot <- ggplot(filter(GemsFinalCoded, block_ix == 1)) +
  aes(instructions_label, score) +
  geom_point(aes(color = instructions_label), position = position_jitter(width = 0.1)) +
  labs(x = "", y = "final score in block 1") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1))

# diff ----
instructions_versus_no_instructions_plot <- ggplot() +
  aes(trial, score) +
  geom_line(aes(color = instructions_label), data = Gen2Block1,
            stat = "summary", fun.y = "mean", size = 1.4) +
  geom_line(aes(group = 1), data = Gen1Block1, stat = "summary", fun.y = "mean",
            linetype = "longdash") +
  scale_color_discrete("") +
  guides(color = guide_legend(reverse = TRUE))

Block1Diff <- bind_rows(
  left_join(Gen1Block1, InstructionsSummarized),
  Gen2Block1
) %>%
  group_by(generation, instructions_label, trial) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  group_by(instructions_label, trial) %>%
  summarize(diff = score[generation == 2] - score[generation == 1]) %>%
  ungroup()

diff_plot <- ggplot(Block1Diff) +
  aes(trial, diff) +
  geom_line(aes(color = instructions_label), size = 1.4) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_color_discrete("") +
  scale_y_continuous("score diff (gen2-gen1)") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.position = "right")

overall_diff_plot <- ggplot(Block1Diff) +
  aes(instructions_label, diff) +
  geom_bar(aes(fill = instructions_label), stat = "summary", fun.y = "mean",
           show.legend = FALSE) +
  # geom_point(aes(color = instructions_label), position = position_jitter(width=0.1, height=0)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete("") +
  scale_y_continuous("score diff (gen2-gen1)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# instructions ---
InstructionsScored <- Instructions %>%
  left_join(InstructionsSummarized)

# filter(InstructionsScored, instructions_score == 0)$instructions

InstructionsBarWidth <- InstructionsCoded %>%
  filter(dimension == "Bar width") %>%
  rename(inherit_from = subj_id, instructions_score = score) %>%
  ungroup()

InstructionsOrientation <- InstructionsCoded %>%
  filter(dimension == "Orientation") %>%
  rename(inherit_from = subj_id, instructions_score = score) %>%
  ungroup()

valid_subjs <- Gems %>%
  count(subj_id) %>%
  mutate(is_valid = n == 160) %>%
  filter(is_valid == 1) %>%
  .$subj_id

GemsGen2 <- Gems %>%
  filter(version == 1.3, generation == 2, subj_id %in% valid_subjs)

generation_map <- unique(GemsGen2[,c("subj_id", "inherit_from")])

canalization_bar_width_plot <- GemsGen2 %>%
  left_join(InstructionsBarWidth) %>%
  group_by(instructions_score, trial) %>%
  summarize(current_x = mean(current_x),
            current_y = mean(current_y),
            selected_x = mean(selected_x),
            selected_y = mean(selected_y)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = current_x, y = current_y, xend = selected_x, yend = selected_y) +
  geom_segment(aes(color = factor(instructions_score))) +
  scale_color_discrete("", labels = c("Didn't mention", "Incomplete or inaccurate", "Correct")) +
  labs(x = "orientation", y = "bar width") +
  theme(legend.position = "top")

canalization_orientation_plot <- GemsGen2 %>%
  left_join(InstructionsOrientation) %>%
  group_by(instructions_score, trial) %>%
  summarize(current_x = mean(current_x),
            current_y = mean(current_y),
            selected_x = mean(selected_x),
            selected_y = mean(selected_y)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = current_x, y = current_y, xend = selected_x, yend = selected_y) +
  geom_segment(aes(color = factor(instructions_score))) +
  scale_color_discrete("", labels = c("Didn't mention", "Incomplete or inaccurate", "Correct")) +
  labs(x = "orientation", y = "bar width") +
  theme(legend.position = "top")

