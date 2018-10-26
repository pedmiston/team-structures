library(tidyverse)

# ---- types-of-time
# Plot the relationship between labor time, calendar time, and learning time.

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