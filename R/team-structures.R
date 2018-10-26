# ---- team-structures
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()
t_$annotation_size <- 2.5  # for manuscript
# t_$annotation_size <- 5      # for slides
theme_set(t_$base_theme)

results <- list()

data("Sessions")

Participants <- Sessions %>%
  filter_50min() %>%
  count(Strategy) %>%
  rename(Participants = n)

Teams <- Sessions %>%
  filter_50min() %>%
  select(Strategy, TeamID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(Teams = n)

N <- left_join(Participants, Teams)

results$n_participants <- sum(N$Participants)

# Number of innovations ----
data("Guesses")

Innovations <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

num_innovations_50min_mod <- lmer(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2 + (1|TeamID),
  data = Innovations
)

results$DG2_v_DG1 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_DG1", reverse_sign = TRUE)
results$DG2_v_I50 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_I50", reverse_sign = TRUE)
results$S2_v_DG2 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_S2", reverse_sign = FALSE)

num_innovations_50min_s2_treat_mod <- lmer(
  NumInnovations ~ S2_v_DG1 + S2_v_DG2 + S2_v_I50 + (1|TeamID),
  data = Innovations
)

results$S2_v_I50 <- report_lmer_mod(num_innovations_50min_s2_treat_mod, "S2_v_I50")

num_innovations_50min_teamwork_mod <- lmer(
  NumInnovations ~ DSvI + DvS + (1|TeamID),
  data = filter(Innovations, SessionType != "DG1")
)

results$teamwork_stats <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DSvI", reverse_sign = TRUE)
results$teamwork_residual <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DvS")

# Use lm mod for error on plot because lmer mod preds with AICcmodavg look too small
num_innovations_50min_lm_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
  data = Innovations
)

num_innovations_50min_preds <- recode_session_type_50min() %>%
  cbind(., predict(num_innovations_50min_lm_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

set.seed(432)
num_innovations_50min_plot <- ggplot(Innovations) +
  aes(SessionTypeSimple, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = num_innovations_50min_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3), shape = 1) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = num_innovations_50min_preds) +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  t_$scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Rate of innovation ----
data("Sampled")

Sampled50min <- Sampled %>%
  filter_50min() %>%
  recode_strategy() %>%
  label_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6)

labels <- recode_session_type_50min() %>%
  mutate(TeamTime = c(12.5, 43.75, 43.75, 43.75),
         NumInnovations = c(4.5, 8.7, 6.8, 10)) %>%
  recode_strategy()

innovation_rate_50min_plot <- ggplot(Sampled50min) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  geom_text(aes(label = SessionTypeSimple, color = StrategyLabel),
            data = labels, fontface = "bold", size = t_$annotation_size) +
  t_$scale_x_team_time +
  t_$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  t_$scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  theme(legend.position = "none")

# Guesses per item ----
data("Guesses")
data("AdjacentItems")
data("Teams")

SessionTypes50min <- Sessions %>%
  filter_50min() %>%
  recode_session_type_50min() %>%
  # Collapse Synchronic players into a single team,
  # but leave Diachronic and Isolated players alone.
  select(Strategy, SessionType, TeamID, Generation) %>%
  unique()

GuessesPerItem50min <- Guesses %>%
  filter_50min() %>%

  # Treat all Synchronic players as "playing" even if they are lagging.
  mutate(Stage = ifelse(Strategy == "Synchronic", "playing", Stage)) %>%

  # Copy guesses for each adjacent item
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  recode_session_type_50min()

CostPerItem50min <- GuessesPerItem50min %>%
  filter(Strategy != "Diachronic" | (Generation == 2 & Stage == "learning")) %>%
  # Summarize the cost of each item for each session type: DG1, DG2, I50, S2.
  # Cost scores are summed across both synchronic players.
  group_by(TeamID, Generation, Adjacent, SessionType) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance() %>%
  recode_strategy()

# Guesses per item by inheritance.
guesses_per_item_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance + (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50min, Discovered)
)
results$guesses_per_item_inheritance <- report_lmer_mod(guesses_per_item_inheritance_mod,
                                                     "Diachronic_v_NoInheritance")

guesses_per_item_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_item_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

# Guesses per item by strategy.
guesses_per_item_treatment_mod <- lmer(
  TotalGuesses ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 +
    (DG2_v_DG1 + DG2_v_S2 + DG2_v_I50|Adjacent),
  data = filter(CostPerItem50min, Discovered))

guesses_per_item_treatment_preds <- recode_session_type_50min() %>%
  cbind(., predictSE(guesses_per_item_treatment_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_treatment_means <- guesses_per_item_treatment_preds %>%
  filter(SessionType != "DG2") %>%
  mutate(Inheritance = "no_inheritance") %>%
  recode_inheritance() %>%
  recode_strategy()

guesses_per_item_treatment_means_2 <- guesses_per_item_treatment_preds %>%
  filter(SessionType == "DG2") %>%
  mutate(Inheritance = "diachronic_inheritance") %>%
  recode_inheritance() %>%
  recode_strategy()

guesses_per_item_plot <- ggplot(CostPerItem50min) +
  aes(InheritanceShort, TotalGuesses) +
  geom_line(aes(group = Adjacent), stat = "summary", fun.y = "mean",
            color = "gray") +
  geom_line(aes(group = 1), data = guesses_per_item_inheritance_preds,
            size = 0.8) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_item_inheritance_preds,
                width = 0.075, size = 0.8) +
  geom_point(aes(color = StrategyLabel),
             data = guesses_per_item_treatment_means, x = 0.9) +
  geom_text(aes(label = SessionType, color = StrategyLabel),
            hjust = 1, size = 3,
            data = guesses_per_item_treatment_means, x = 0.86) +
  geom_point(aes(color = StrategyLabel),
             data = guesses_per_item_treatment_means_2, x = 2.1) +
  geom_text(aes(label = SessionType, color = StrategyLabel),
            hjust = 0, size = 3,
            data = guesses_per_item_treatment_means_2, x = 2.14) +
  xlab("") +
  scale_y_continuous("Guesses per innovation", breaks = seq(0, 300, by = 50)) +
  coord_cartesian(xlim = c(0.65, 2.35), ylim = c(0, 200), expand = FALSE) +
  t_$scale_color_strategy +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# Guesses per item: Playing ----
CostPerItem50minPlaying <- GuessesPerItem50min %>%
  filter(Strategy != "Diachronic" | (Generation == 2 & Stage == "playing")) %>%
  # Summarize the cost of each item for each session type: DG1, DG2, I50, S2.
  # Cost scores are summed across both synchronic players.
  group_by(TeamID, Generation, Adjacent, SessionType) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance()

guesses_per_new_item_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance + (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50minPlaying, Discovered)
)
results$guesses_per_new_item_inheritance <- report_lmer_mod(guesses_per_new_item_inheritance_mod,
                                                     "Diachronic_v_NoInheritance")
guesses_per_new_item_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_new_item_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

# Guesses per item by strategy.
guesses_per_new_item_treatment_mod <- lmer(
  TotalGuesses ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 +
    (DG2_v_DG1 + DG2_v_S2 + DG2_v_I50|Adjacent),
  data = filter(CostPerItem50minPlaying, Discovered))

guesses_per_new_item_treatment_preds <- recode_session_type_50min() %>%
  cbind(., predictSE(guesses_per_new_item_treatment_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_new_item_treatment_means <- guesses_per_new_item_treatment_preds %>%
  filter(SessionType != "DG2") %>%
  mutate(Inheritance = "no_inheritance") %>%
  recode_inheritance() %>%
  recode_strategy()

guesses_per_new_item_treatment_means_2 <- guesses_per_new_item_treatment_preds %>%
  filter(SessionType == "DG2") %>%
  mutate(Inheritance = "diachronic_inheritance") %>%
  recode_inheritance() %>%
  recode_strategy()

guesses_per_new_item_plot <- ggplot(guesses_per_new_item_treatment_mod) +
  aes(InheritanceShort, TotalGuesses) +
  geom_line(aes(group = Adjacent), stat = "summary", fun.y = "mean",
            color = "gray") +
  geom_line(aes(group = 1), data = guesses_per_new_item_inheritance_preds,
            size = 0.8) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_new_item_inheritance_preds,
                width = 0.075, size = 0.8) +
  geom_point(aes(color = StrategyLabel),
             data = guesses_per_new_item_treatment_means, x = 0.9) +
  geom_text(aes(label = SessionType, color = StrategyLabel),
            hjust = 1, size = 3,
            data = guesses_per_new_item_treatment_means, x = 0.86) +
  geom_point(aes(color = StrategyLabel),
             data = guesses_per_new_item_treatment_means_2, x = 2.1) +
  geom_text(aes(label = SessionType, color = StrategyLabel),
            hjust = 0, size = 3,
            data = guesses_per_new_item_treatment_means_2, x = 2.14) +
  xlab("") +
  scale_y_continuous("Guesses per innovation", breaks = seq(0, 300, by = 50)) +
  coord_cartesian(xlim = c(0.65, 2.375), ylim = c(0, 200), expand = FALSE) +
  t_$scale_color_strategy +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )


# Guess types ----
GuessTypes <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  label_inheritance() %>%
  recode_inheritance()

prop_redundant_inheritance_mod <- lm(PropRedundantGuesses ~ Diachronic_v_NoInheritance,
                                     data = GuessTypes)
results$prop_redundant_inheritance <- report_lm_mod(prop_redundant_inheritance_mod,
                                                 "Diachronic_v_NoInheritance")

prop_redundant_guesses_mod <- lm(PropRedundantGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                                 data = GuessTypes)
results$prop_redundant_dg2 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_DG1")
results$prop_redundant_i50 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_I50")
results$prop_redundant_s2 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_S2")

prop_unique_inheritance_mod <- lm(PropUniqueGuesses ~ Diachronic_v_NoInheritance,
                                  data = GuessTypes)
results$prop_unique_inheritance <- report_lm_mod(prop_unique_inheritance_mod, "Diachronic_v_NoInheritance")

prop_unique_guesses_mod <- lm(PropUniqueGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                              data = GuessTypes)
results$prop_unique_dg2 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_DG1")
results$prop_unique_i50 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_I50")
results$prop_unique_s2 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_S2")

GuessTypes50minSummary <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  recode_session_type_50min() %>%
  group_by(SessionTypeSimple) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  recode_session_type_50min() %>%
  select(SessionTypeSimple, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -SessionTypeSimple) %>%
  recode_session_type_50min() %>%
  recode_prop_guess_type_total()

prop_guess_types_50min_plot <- ggplot(GuessTypes50minSummary) +
  aes(SessionTypeSimple, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  xlab("") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  theme(panel.grid.major.x = element_blank())

# Guessing rates ----
data("Sampled")
sampled_interval <- 1 # Sampled data was sampled at 1 minute intervals


GuessingRates <- Sampled %>%
  filter_50min() %>%
  mutate(NumRedundant = NumGuesses - NumUniqueGuesses) %>%
  group_by(SessionID) %>%
  mutate(
    NewItems = (InventorySize - lag(InventorySize)),
    NewItemRate = NewItems/sampled_interval,
    GuessDiff = NumGuesses - lag(NumGuesses),
    GuessRate = GuessDiff/sampled_interval,
    UniqueDiff = NumUniqueGuesses - lag(NumUniqueGuesses),
    UniqueRate = UniqueDiff/sampled_interval,
    RedundantDiff = NumRedundant - lag(NumRedundant),
    RedundancyRate = RedundantDiff/sampled_interval
  ) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance() %>%
  recode_session_time()


new_items_per_minute_mod <- lm(NewItems ~ (SessionTimeZ + SessionTimeZ_2 + SessionTimeZ_3) * (DG2_v_DG1 + DG2_v_I50 + DG2_v_S2),
                               data = GuessingRates)
new_items_per_minute_preds <- expand.grid(
  SessionType = c("DG1", "DG2", "I50", "S2"),
  SessionTime = 0:50,
  stringsAsFactors = FALSE
) %>%
  recode_session_time() %>%
  filter(SessionType == "I50" | SessionTime <= 25) %>%
  recode_session_type_50min() %>%
  cbind(., predict(new_items_per_minute_mod, newdata = ., se = TRUE)) %>%
  as_data_frame() %>%
  rename(NewItemRate = fit, SE = se.fit) %>%
  recode_strategy()

guessing_rate_plot <- ggplot(GuessingRates) +
  aes(SessionTime) +
  geom_point(aes(group = SessionType, color = Strategy, shape = Inheritance),
             stat = "summary", fun.y = "mean") +
  xlab("Session time (minutes)") +
  scale_shape_manual(values = c(1, 16), guide = "none") +
  t_$scale_color_strategy

new_items_per_minute <- guessing_rate_plot +
  aes(y = NewItemRate) +
  # geom_ribbon(aes(group = SessionType, fill = Strategy, ymin = NewItemRate-SE, ymax = NewItemRate+SE),
  #             data = new_items_per_minute_preds,
  #             alpha = 0.6) +
  t_$scale_fill_strategy

guesses_per_minute <- guessing_rate_plot +
  aes(y = GuessRate) +
  geom_hline(yintercept = mean(GuessingRates$GuessRate, na.rm = TRUE), linetype = 2)

unique_per_minute <- guessing_rate_plot +
  aes(y = UniqueRate) +
  ylab("")

redundant_per_minute <- guessing_rate_plot +
  aes(y = RedundancyRate)
