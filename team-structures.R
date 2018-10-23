# ---- team-structures ----
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()
# t_$annotation_size <- 2.5  # for manuscript
t_$annotation_size <- 5      # for slides
theme_set(t_$base_theme)

# * Intro ----
# Makes "types of time" plot.
# Types of time plot shows the relationship
# between labor time, calendar time, and learning time.

diachronic <- data_frame(
  Strategy = "Diachronic",
  CalendarHours = 1:100,
  LaborHours = CalendarHours,
  Person = rep(c(1, 2), each = 50)
)

isolated <- diachronic %>%
  mutate(
    Strategy = "Isolated",
    Person = 1
  )

synchronic <- data_frame(
  Strategy = "Synchronic",
  CalendarHours = 1:50,
  LaborHours = CalendarHours * 2,
  Person = 1
)

time <- rbind(diachronic, synchronic, isolated) %>%
  group_by(Strategy, Person) %>%
  mutate(PersonHours = 1:n()) %>%
  ungroup() %>%
  totems::recode_strategy() %>%
  mutate(
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                        ifelse(Strategy == "Diachronic", LaborHours + 1, LaborHours - 1))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

labels <- totems::recode_strategy() %>%
  mutate(
    CalendarHours = 25,
    LaborHours = c(60, 18, 35),
    Angle = c(67.5, 47, 47)
  )

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  geom_text(aes(label = StrategyLabel, angle = Angle), data = labels, hjust = 0, size = 2.5) +
  scale_x_continuous("Calendar time", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_color_strategy +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = "none", linetype = "none") +
  t_$base_theme

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

labels <- totems::recode_strategy() %>%
  mutate(
    StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")),
    PersonHours = 5
  )

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  geom_text(aes(label = StrategyLabel), angle = 90, data = labels, hjust = 0, size = 2.5) +
  scale_x_discrete("", labels = NULL) +
  scale_y_continuous("Learning time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())


# * Methods ----

data("Teams")
data("Sessions")

methods <- list()  # Store vars for in-text reference
methods$n_unique_combinations_6 <- count_unique_combinations(6)
methods$n_unique_combinations_6_pct <- round(3/methods$n_unique_combinations_6 * 100, 1)

TeamCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumTeams = n)

PlayerCounts <- Sessions %>%
  left_join(
    Teams %>%
      select(TeamID, SessionDuration, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(PlayerID, Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  mutate(`Sessions` = c(1, 1, 4, 1, 1)) %>%
  left_join(TeamCounts) %>%
  left_join(PlayerCounts) %>%
  rename(`Duration (min)` = SessionDuration, `Session size` = PlayersPerSession,
         `Teams` = NumTeams, `Participants` = NumPlayers)

# * 50min ----

# List to hold descriptives for in-text citation
exp1 <- list()

# ** Methods ----
data("Sessions")

Exp2Participants <- Sessions %>%
  filter_50min() %>%
  count(Strategy) %>%
  rename(`$N_{participants}$` = n)

Exp1Teams <- Sessions %>%
  filter_50min() %>%
  select(Strategy, TeamID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(`$N_{teams}$` = n)

Exp1N <- left_join(Exp2Participants, Exp1Teams)

exp1$n_participants <- sum(Exp1N$`$N_{participants}$`)

# ** Number of innovations ----
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

exp1$DG2_v_DG1 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_DG1", reverse_sign = TRUE)
exp1$DG2_v_I50 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_I50", reverse_sign = TRUE)
exp1$S2_v_DG2 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_S2", reverse_sign = FALSE)

num_innovations_50min_s2_treat_mod <- lmer(
  NumInnovations ~ S2_v_DG1 + S2_v_DG2 + S2_v_I50 + (1|TeamID),
  data = Innovations
)

exp1$S2_v_I50 <- report_lmer_mod(num_innovations_50min_s2_treat_mod, "S2_v_I50")

num_innovations_50min_teamwork_mod <- lmer(
  NumInnovations ~ DSvI + DvS + (1|TeamID),
  data = filter(Innovations, SessionType != "DG1")
)

exp1$teamwork_stats <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DSvI", reverse_sign = TRUE)
exp1$teamwork_residual <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DvS")

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

# ** Rate of innovation ----
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

# ** Guesses per item ----
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
exp1$guesses_per_item_inheritance <- report_lmer_mod(guesses_per_item_inheritance_mod,
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

# ** Guesses per item: Playing ----
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
exp1$guesses_per_new_item_inheritance <- report_lmer_mod(guesses_per_new_item_inheritance_mod,
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


# ** Guess types ----
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
exp1$prop_redundant_inheritance <- report_lm_mod(prop_redundant_inheritance_mod,
                                                 "Diachronic_v_NoInheritance")

prop_redundant_guesses_mod <- lm(PropRedundantGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                                 data = GuessTypes)
exp1$prop_redundant_dg2 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_DG1")
exp1$prop_redundant_i50 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_I50")
exp1$prop_redundant_s2 <- report_lm_mod(prop_redundant_guesses_mod, "DG2_v_S2")

prop_unique_inheritance_mod <- lm(PropUniqueGuesses ~ Diachronic_v_NoInheritance,
                                  data = GuessTypes)
exp1$prop_unique_inheritance <- report_lm_mod(prop_unique_inheritance_mod, "Diachronic_v_NoInheritance")

prop_unique_guesses_mod <- lm(PropUniqueGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                              data = GuessTypes)
exp1$prop_unique_dg2 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_DG1")
exp1$prop_unique_i50 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_I50")
exp1$prop_unique_s2 <- report_lm_mod(prop_unique_guesses_mod, "DG2_v_S2")

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

# ** Guessing rates ----
data("Sampled")
sampled_interval <- 1 # Sampled data was sampled at 1 minute intervals

recode_session_time <- function(frame) {
  session_time_map <- data_frame(
    SessionTime = 0:50,
    SessionTimeC = -25:25,
    SessionTimeZ = (SessionTimeC - mean(SessionTimeC))/sd(SessionTimeC),
    SessionTimeZ_2 = SessionTimeZ^2,
    SessionTimeZ_3 = SessionTimeZ^3
  )
  if(missing(frame)) return(session_time_map)
  left_join(frame, session_time_map)
}

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

# * SelfOther ----

exp2 <- list()

# ** Methods ----
data("Sessions")

Exp3Participants <- Sessions %>%
  filter_selfother() %>%
  select(Strategy, PlayerID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(N = n)

# ** Innovations by generation ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_selfother() %>%
  recode_guess_type(unique_guess = "UniqueSessionGuess", unique_result = "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  recode_strategy()

innovations_gen1_mod <- lm(NumInnovations ~ Diachronic_v_Isolated,
                           data = filter(Innovations, Generation == 1))
exp2$gen1_innovations <- report_lm_mod(innovations_gen1_mod, "Diachronic_v_Isolated")

diachronic_gen_mod <- lmer(NumInnovations ~ Generation + (Generation|TeamID),
                           data = filter(Innovations, Strategy == "Diachronic"))
exp2$tools_per_diachronic_gen <- report_beta(diachronic_gen_mod, "Generation")
exp2$tools_per_diachronic_gen_stats <- report_lmer_mod(diachronic_gen_mod, "Generation")

isolated_gen_mod <- lmer(NumInnovations ~ Generation + (Generation|TeamID),
                         data = filter(Innovations, Strategy == "Isolated"))
exp2$tools_per_isolated_gen <- report_beta(isolated_gen_mod, "Generation")
exp2$tools_per_isolated_gen_stats <- report_lmer_mod(isolated_gen_mod, "Generation")

innovations_gen4_mod <- lm(NumInnovations ~ Diachronic_v_Isolated,
                           data = filter(Innovations, Generation == 4))
exp2$gen4_innovations <- report_lm_mod(innovations_gen4_mod, "Diachronic_v_Isolated")

innovations_by_generation_mod <- lmer(
  NumInnovations ~ Generation * Diachronic_v_Isolated  + (Generation|TeamID),
  data = Innovations
)

exp2$innovations_by_inheritance_slope <- report_lmer_mod(innovations_by_generation_mod, "Generation:Diachronic_v_Isolated")

innovations_by_generation_quad_mod <- lmer(
  NumInnovations ~ (Generation0 + Generation0Sqr) * Diachronic_v_Isolated + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_preds <- expand.grid(
    Generation = 1:4, Strategy = c("Diachronic", "Isolated"),
    stringsAsFactors = FALSE
  ) %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  recode_strategy() %>%
  cbind(., predictSE(innovations_by_generation_quad_mod, newdata = ., SE = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

innovations_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations, color = StrategyLabel) +
  geom_line(aes(GenerationJittered, group = TeamID, color = Strategy),
            size = 0.8, alpha = 0.6) +
  geom_ribbon(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE, fill = Strategy),
                data = innovations_by_generation_preds, alpha = 0.4,
              size = 0) +
  facet_wrap("Strategy") +
  scale_color_manual(values = t_$color_picker(c("blue", "orange"))) +
  scale_fill_manual(values = t_$color_picker(c("blue", "orange"))) +
  scale_y_continuous("Number of innovations", breaks = seq(0, 40, by = 5)) +
  theme(
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# ** Guess types ----
GuessTypesSelfOther <- Guesses %>%
  filter_selfother() %>%
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
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  recode_strategy()

prop_redundant_strategy_mod <- lmer(PropRedundantGuesses ~ Diachronic_v_Isolated + (1|TeamID),
                                    data = GuessTypesSelfOther)
exp2$prop_redundant_strategy <- report_lmer_mod(prop_redundant_strategy_mod, "Diachronic_v_Isolated")

prop_redundant_gen_mod <- lmer(PropRedundantGuesses ~ Generation + (1|TeamID),
                               data = GuessTypesSelfOther)
exp2$prop_redundant_gen <- report_lmer_mod(prop_redundant_gen_mod, "Generation", formats = c(se=3))

prop_redundant_inter_mod <- lmer(PropRedundantGuesses ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                               data = GuessTypesSelfOther)
exp2$prop_redundant_inter <- report_lmer_mod(prop_redundant_gen_mod, "Generation:Diachronic_v_Isolated", formats = c(b = 3))


prop_unique_strategy_mod <- lmer(PropUniqueGuesses ~ Diachronic_v_Isolated + (1|TeamID),
                              data = GuessTypesSelfOther)
exp2$prop_unique_strategy <- report_lmer_mod(prop_unique_strategy_mod, "Diachronic_v_Isolated")

prop_unique_gen_mod <- lmer(PropUniqueGuesses ~ Generation + (Generation|TeamID),
                              data = GuessTypesSelfOther)
exp2$prop_unique_gen <- report_lmer_mod(prop_unique_gen_mod, "Generation", formats = c(b=3))

prop_unique_inter_mod <- lmer(PropUniqueGuesses ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                            data = GuessTypesSelfOther)
exp2$prop_unique_inter <- report_lmer_mod(prop_unique_inter_mod, "Generation:Diachronic_v_Isolated")


GuessTypesSelfOtherSummary <- Guesses %>%
  filter_selfother() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(Strategy, Generation) %>%
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
  select(Strategy, Generation, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -c(Strategy, Generation)) %>%
  recode_prop_guess_type_total()

prop_guess_types_selfother_plot <- ggplot(GuessTypesSelfOtherSummary) +
  aes(Generation, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  facet_wrap("Strategy") +
  xlab("") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  theme(panel.grid.major.x = element_blank())

# ** Learning times ----
StageTimesSelfOther <- Guesses %>%
  filter_selfother() %>%
  filter(Generation > 1) %>%
  group_by(SessionID) %>%
  summarize(LearningTime = max(SessionTime[Stage == "learning"])) %>%
  mutate(PlayingTime = 25 - LearningTime) %>%
  left_join(Sessions) %>%
  recode_strategy()

exp2$mean_learning_time <- round(mean(StageTimesSelfOther$LearningTime), 1)
exp2$prop_learning_time <- round((mean(StageTimesSelfOther$LearningTime)/25) * 100, 1)

learning_times_mod <- lmer(LearningTime ~ Generation * Diachronic_v_Isolated + (Generation|TeamID),
                           data = StageTimesSelfOther)
exp2$learning_times_inter <- report_lmer_mod(learning_times_mod, "Generation:Diachronic_v_Isolated")

learning_times_preds <- expand.grid(
  Generation = 2:4,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(learning_times_mod, newdata = ., se = TRUE)) %>%
  rename(LearningTime = fit, SE = se.fit)

learning_times_plot <- ggplot(StageTimesSelfOther) +
  aes(Generation, LearningTime) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", data = learning_times_preds, alpha = 0.6) +
  geom_point(aes(color = StrategyLabel), position = position_jitter(width=0.1, height=0)) +
  geom_errorbar(aes(ymin = LearningTime-SE, ymax = LearningTime+SE),
                data = learning_times_preds, width = 0.2) +
  facet_wrap("Strategy") +
  scale_y_continuous("Learning time (min)") +
  scale_fill_manual(values = t_$color_picker(c("orange", "blue"))) +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# ** Guesses per item ----

# ** Fixation ----
data("Guesses")
data("Sessions")
data("AdjacentItems")

IndividualPlayers <- Sessions %>%
  filter_selfother()

IndividualGuesses <- Guesses %>%
  filter_selfother() %>%
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  label_stage_ix()

FirstDiscovery <- IndividualGuesses %>%
  label_inheritance() %>%
  filter(StageIX == 0, Inheritance != "no_inheritance") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  ungroup() %>%
  left_join(IndividualPlayers) %>%
  highlight_inheritance_100() %>%
  # Should not be necessary!
  filter(!is.na(Inheritance)) %>%
  recode_inheritance() %>%
  recode_strategy()

first_discovery_mod <- lm(NumGuesses ~ Diachronic_v_Individual,
                          data = FirstDiscovery)
exp2$first_discovery_mean <- round(mean(FirstDiscovery$NumGuesses), 0)
exp2$first_discovery_beta <- report_beta(first_discovery_mod, "Diachronic_v_Individual")
exp2$first_discovery_stats <- report_lm_mod(first_discovery_mod, "Diachronic_v_Individual")

first_discovery_preds <- recode_inheritance() %>%
  filter(Inheritance != "no_inheritance") %>%
  mutate(Strategy = c("Diachronic", "Isolated")) %>%
  cbind(., predict(first_discovery_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

first_discovery_plot <- ggplot(FirstDiscovery) +
  aes(StrategyLabel, NumGuesses) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                width = 0.2, data = first_discovery_preds) +
  scale_y_continuous("Number of guesses") +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

first_discovery_by_generation_mod <- lmer(NumGuesses ~ Diachronic_v_Individual * Generation +
                                            (Generation|TeamID),
                                          data = FirstDiscovery)
exp2$first_discovery_by_gen_stats <- report_lmer_mod(first_discovery_by_generation_mod,
                                                     "Diachronic_v_Individual:Generation")

first_discovery_by_generation_preds <- expand.grid(
    Generation = 2:4, Inheritance = c("diachronic_inheritance", "individual_inheritance"),
    stringsAsFactors = FALSE
  ) %>%
  recode_inheritance() %>%
  mutate(Strategy = rep(c("Diachronic", "Isolated"), each = 3)) %>%
  recode_strategy() %>%
  cbind(., predictSE(first_discovery_by_generation_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

first_discovery_by_generation_plot <- ggplot(FirstDiscovery) +
  aes(Generation, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.1, height = 0)) +
  geom_bar(aes(fill = StrategyLabel, group = factor(Generation)),
           data = first_discovery_by_generation_preds,
           stat = "identity",
           alpha = 0.6, width = 0.8) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                data = first_discovery_by_generation_preds,
                width = 0.2) +
  facet_wrap("Strategy") +
  scale_x_continuous(breaks = 2:4) +
  scale_y_continuous("Number of guesses") +
  scale_fill_manual(values = t_$color_picker(c("orange", "blue"))) +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# * TeamSize ----

exp3 <- list()

# ** Methods ----

# ** Number of innovations ----
data("Guesses")

drop_isolated <- . %>% filter(Strategy != "Isolated")

recode_strategy_diachronic_v_synchronic <- function(frame) {
  diachronic_v_synchronic <- data_frame(
    Strategy = c("Diachronic", "Synchronic"),
    Diachronic_v_Synchronic_C = c(-0.5, 0.5)
  )
  if(missing(frame)) return(diachronic_v_synchronic)
  left_join(frame, diachronic_v_synchronic)
}

Innovations50min <- Guesses %>%
  filter_50min() %>%
  drop_isolated() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(NumPlayers = 2)

Innovations100min <- Guesses %>%
  filter_teamsize() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(NumPlayers = 4)

Innovations <- bind_rows(Innovations50min, Innovations100min) %>%
  group_by(Strategy, NumPlayers, TeamID) %>%
  summarize(NumInnovations = max(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_team_size() %>%
  recode_strategy_diachronic_v_synchronic()

max_innovations_50min_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic_C,
  data = filter(Innovations, NumPlayers == 2)
)

max_innovations_100min_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic_C,
  data = filter(Innovations, NumPlayers == 4)
)

max_innovations_synchronic <- lm(
  NumInnovations ~ NumPlayers,
  data = filter(Innovations, Strategy == "Synchronic")
)

max_innovations_diachronic <- lm(
  NumInnovations ~ NumPlayers,
  data = filter(Innovations, Strategy == "Diachronic")
)

max_innovations_by_teamsize_mod <- lmer(
  NumInnovations ~ Diachronic_v_Synchronic * NumPlayers + (1|TeamID),
  data = Innovations)

exp3$DvS_2 <- report_lm_mod(max_innovations_50min_mod, "Diachronic_v_Synchronic_C")
exp3$DvS_4 <- report_lm_mod(max_innovations_100min_mod, "Diachronic_v_Synchronic_C")
exp3$S_2v4 <- report_lm_mod(max_innovations_synchronic, "NumPlayers")
exp3$D_2v4 <- report_lm_mod(max_innovations_diachronic, "NumPlayers")
exp3$DvS_2v4 <- report_lmer_mod(max_innovations_by_teamsize_mod, "Diachronic_v_Synchronic:NumPlayers")

max_innovations_by_teamsize_preds <- expand.grid(
  Strategy = c("Diachronic", "Synchronic"),
  NumPlayers = c(2, 4),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(max_innovations_by_teamsize_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_team_size()

set.seed(432)
max_innovations_by_teamsize_plot <- ggplot(Innovations) +
  aes(factor(NumPlayers), NumInnovations) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", alpha = 0.6,
           data = max_innovations_by_teamsize_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3, height = 0)) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = max_innovations_by_teamsize_preds, width=0.3) +
    facet_wrap("StrategyLabel") +
  scale_x_discrete("Group size") +
  scale_fill_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  scale_color_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  scale_y_continuous("Number of innovations", breaks = seq(0, 30, by = 2), expand = c(0, 0), limits = c(0, 27)) +
  # coord_cartesian(ylim)
  guides(color = "none", fill = "none") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  xlab("")

# ** BotsPlayers ----
data("BotsPlayers")

sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()

min_players <- min(BotsPlayers$n_players)
max_players <- max(BotsPlayers$n_players)
min_guesses <- min(BotsPlayers$n_guesses)
max_guesses <- max(BotsPlayers$n_guesses)

BotsPlayersFinal <- BotsPlayers %>%
  filter((n_players == min_players & n_guesses == min_guesses) | (n_players == max_players & n_guesses == max_guesses)) %>%
  filter_final_round() %>%
  mutate(num_innovations = inventory_size - 6) %>%
  mutate(Strategy = str_to_title(strategy)) %>%
  recode_strategy()

bots_team_size_plot <- ggplot(BotsPlayersFinal) +
  aes(factor(n_players), num_innovations) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean", alpha = 0.6) +
  # geom_point(aes(color = StrategyLabel), position = position_jitter(width = 0.3)) +
  facet_wrap("StrategyLabel") +
  scale_x_discrete("Group size") +
  scale_y_continuous("Number of innovations", breaks = 0:10) +
  scale_fill_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  scale_color_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
