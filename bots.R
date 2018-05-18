source("docs/R/0-setup.R")

# ---- simulations

# Scale inventory size as number of innovations
t_$scale_y_inventory_size <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(6, 16, by = 2),
  labels = seq(0, 10, by = 2)
)

# Scales for diachronic/synchronic colors
t_$scale_color_strategy_2 <- scale_color_manual(
  "Strategy",
  labels = c("Diachronic", "Synchronic"),
  values = t_$color_picker(c("blue", "green"))
)
t_$scale_fill_strategy_2 <- scale_fill_manual(
  "Strategy",
  labels = c("Diachronic", "Synchronic"),
  values = t_$color_picker(c("blue", "green"))
)

sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()


# ---- simulations-strategy
# BotsStrategy ----
data("BotsStrategy")

BotsStrategy %<>%
  mutate(strategy_rev = factor(strategy, levels = c("synchronic", "diachronic")))

timeline_plot <- ggplot(BotsStrategy) +
  aes(round, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
  geom_vline(xintercept = 49, linetype = 2, size = 0.3) +
  annotate("text", x = 49, y = 14, label = "calendar hours", angle = 90,
           vjust = -0.2, size = 3) +
  scale_x_continuous("Round") +
  t_$scale_y_inventory_size +
  t_$scale_color_strategy_2 +
  coord_cartesian(ylim = c(6, 16)) +
  t_$base_theme +
  theme(legend.position = "top")

final_plot <- ggplot() +
  aes(strategy_rev, inventory_size) +
  geom_bar(aes(fill = strategy), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = strategy), position = position_jitter(0.3),
             shape = 1) +
  scale_x_discrete("Strategy", labels = c("Synchronic", "Diachronic")) +
  t_$scale_y_inventory_size +
  t_$scale_color_strategy_2 +
  t_$scale_fill_strategy_2 +
  coord_cartesian(ylim = c(6, 16)) +
  t_$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

calendar_hours_plot <- final_plot %+% filter(BotsStrategy, round == 49)
labor_hours_plot    <- final_plot %+% filter_final_round(BotsStrategy)

gridExtra::grid.arrange(
  crotchet::read_graphviz("team-structures-2", "totems"),
  timeline_plot +
    theme(legend.position = "none") +
    ggtitle("Performance over time"),
  calendar_hours_plot +
    theme(axis.title.y = element_blank()) +
    ggtitle("Calendar hours"),
  labor_hours_plot +
    theme(axis.title.y = element_blank()) +
    ggtitle("Labor hours"),
  nrow = 1,
  widths = c(0.16, 0.28, 0.28, 0.28)
)


# ---- simulations-team-size
# BotsPlayers ----
data("BotsPlayers")

BotsPlayersFinal <- filter_final_round(BotsPlayers)

ggplot(BotsPlayersFinal) +
  aes(n_players, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1) +
  geom_point(position = position_jitter(1), alpha = 0.2, shape = 1) +
  scale_x_continuous("Team size") +
  t_$scale_y_inventory_size +
  t_$scale_color_strategy_2 +
  t_$base_theme


# ---- simulations-memory
# BotsMemory ----
recode_team_memory <- function(frame) {
  team_memory_levels <- c("no team memory", "yes team memory")
  team_memory_map <- data_frame(
    team_memory = c(0, 1),
    team_memory_label = factor(team_memory_levels, levels = rev(team_memory_levels))
  )
  left_join(frame, team_memory_map)
}

recode_player_memory <- function(frame) {
  player_memory_levels <- c("no player memory", "yes player memory")
  player_memory_map <- data_frame(
    player_memory = c(0, 1),
    player_memory_label = factor(player_memory_levels, levels = player_memory_levels)
  )
  left_join(frame, player_memory_map)
}

recode_memory <- . %>%
  recode_team_memory() %>%
  recode_player_memory()

data("BotsMemory")

BotsMemory %<>%
  recode_memory()

sim_vars <- c("sim_id", "strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
groupby_vars <- c(sim_vars, "inventory", "inventory_size")

BotsInventories <- BotsMemory %>%
  group_by_(.dots = sim_vars) %>%
  summarize(
    n_team_guesses = sum(n_team_guesses),
    n_player_unique_guesses = sum(n_player_unique_guesses),
    n_team_unique_guesses = sum(n_team_unique_guesses)
  ) %>%
  ungroup() %>%
  arrange(sim_id) %>%
  mutate(
    team_redundancy = 1 - n_team_unique_guesses/n_team_guesses,
    player_redundancy = 1 - n_player_unique_guesses/n_team_guesses
  ) %>%
  recode_memory()

redundancy_plot <- ggplot(BotsInventories) +
  geom_point(position = position_jitter(width = 0.2), shape = 1, size = 0.5) +
  geom_point(stat = "summary", fun.y = "mean", size = 3) +
  facet_grid(team_memory_label ~ player_memory_label)

redundancy_plot +
  aes(strategy, player_redundancy) +
  ggtitle("Player redundancy")

redundancy_plot +
  aes(strategy, team_redundancy) +
  facet_grid(player_memory_label ~ team_memory_label) +
  ggtitle("Team redundancy")
