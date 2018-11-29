# ---- totems
library(tidyverse)
library(lme4)
library(AICcmodavg)
library(totems)

# * Number of innovations ----
data("Guesses")
data("ManifestInstructions")

label_instructions_condition <- function(frame) {
  map <- select(ManifestInstructions, PlayerID, Instructions)
  left_join(frame, map)
}

recode_instructions_condition <- function(frame) {
  map <- data_frame(
    Instructions = c("no_instructions", "instructions"),
    InstructionsC = c(-0.5, 0.5),
    InstructionsLabel = factor(c("no_instructions", "instructions"), levels = c("no_instructions", "instructions"),
                               labels = c("No Instructions", "Instructions"))
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}

recode_session_type_instructions <- function(frame) {
  map <- data_frame(
    Generation = c(1, 2, 1, 2),
    Instructions = c("no_instructions", "no_instructions", "instructions", "instructions")
  )
}

Innovations <- Guesses %>%
  filter_instructions() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  label_instructions_condition() %>%
  recode_instructions_condition()

num_innovations_mod <- lmer(NumInnovations ~ Generation * InstructionsC + (1|TeamID),
                            data = Innovations)
num_innovations_preds <- expand.grid(Generation = 1:2, InstructionsC = c(-0.5, 0.5)) %>%
  cbind(., predictSE(num_innovations_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_instructions_condition()


set.seed(342)
num_innovations_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations) +
  geom_line(aes(color = InstructionsLabel, group = TeamID),
            alpha = 0.4) +
  geom_line(aes(color = InstructionsLabel),
            size = 2, data = num_innovations_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE, color = InstructionsLabel),
                data = num_innovations_preds, width = 0.1, size = 2) +
  scale_x_continuous(breaks = 1:2)

# * Rate of innovation ----
data("Sampled")

Sampled <- Sampled %>%
  filter_instructions() %>%
  recode_strategy() %>%
  label_instructions_condition() %>%
  recode_instructions_condition() %>%
  mutate(NumInnovations = InventorySize - 6)

rate_of_innovation_plot <- ggplot(Sampled) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = InstructionsLabel,
                group = interaction(InstructionsLabel, Generation)),
            stat = "summary", fun.y = "mean")
