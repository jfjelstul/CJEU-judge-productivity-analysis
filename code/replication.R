# Joshua C. Fjelstul, Ph.D.

# Load data
load("data/cj_judgments.RData")
load("data/gc_judgments.RData")
load("data/case_duration.RData")

# Analysis ---------------------------------------------------------------------

# Model formula
cj_f <- duration_days ~

  # Procedural complexity
  count_judges + count_joined_proceedings + count_opinions +
  is_appeal + is_direct_action +

  # Legal complexity
  count_cited_documents +
  principles_of_law + national_law + legal_order +
  count_sources_of_law +

  # Policy complexity
  count_subject_keywords + count_policy_areas +

  # Subject matter
  agriculture + budget + competition + consumer_protection +
  customs_union + economic_policy + environment + external_relations +
  home_affairs + industrial_policy + internal_market + justice +
  social_policy + taxation + trade + transport +

  # Judge-Rapporteur
  judge_rapporteur

# Run model
cj_model <- lm(cj_f, data = cj_judgments)
summary(cj_model)

# Model formula
gc_f <- duration_days ~

  # Procedural complexity
  count_judges + count_joined_proceedings + count_procedures +
  is_appeal + is_direct_action +

  # Legal complexity
  count_cited_documents +
  principles_of_law + legal_order +
  count_sources_of_law +

  # Policy complexity
  count_subject_keywords + count_policy_areas +

  # Subject matter
  agriculture + competition +
  industrial_policy + internal_market + trade +

  # Judge-Rapporteur
  judge_rapporteur

# Model GC
gc_model <- lm(gc_f, data = gc_judgments)
summary(gc_model)

# Organize results of analysis ---------------------------------------------------------

# Function to create index
create_index <- function(x) {
  x <- (x - min(x)) / (max(x) - min(x)) * 10
  x <- 10 - x
  return(x)
}

# CJ judge means
cj_judge_means <- cj_judgments |>
  filter(
    judge_rapporteur != "Lenaerts"
  ) |>
  group_by(
    judge_rapporteur, member_state_code, is_current
  ) |>
  summarize(
    average_duration_days = mean(duration_days),
    count_judgments = n(),
    .groups = "drop"
  ) |>
  select(
    judge = judge_rapporteur, is_current, average_duration_days,
    count_judgments, member_state_code
  )

# CJ judge estimates
cj_judge_estimates <- cj_model |>
  tidy() |>
  filter(
    str_detect(term, "judge_rapporteur")
  ) |>
  mutate(
    estimate = estimate,
    index = create_index(estimate),
    judge = term |>
      str_remove("judge_rapporteur"),
    judge = fct_reorder(judge, estimate)
  ) |>
  select(
    judge, estimate, index
  )

# Combine CJ data
cj_data <- cj_judge_means |>
  inner_join(
    cj_judge_estimates,
    by = "judge"
  )

# Calculate variables for CJ judges
cj_data <- cj_data |>
  mutate(
    judge = str_c(judge, " (", member_state_code, ")"),
    rank_average = rank(-average_duration_days),
    rank_index = rank(index)
  )

# GC judge means
gc_judge_means <- gc_judgments |>
  filter(
    judge_rapporteur != "van der Woude"
  ) |>
  group_by(
    judge_rapporteur, member_state_code, is_current
  ) |>
  summarize(
    average_duration_days = mean(duration_days),
    count_judgments = n(),
    .groups = "drop"
  ) |>
  select(
    judge = judge_rapporteur, is_current, average_duration_days,
    count_judgments, member_state_code
  )

# GC judge estimates
gc_judge_estimates <- gc_model |>
  tidy() |>
  filter(
    str_detect(term, "judge_rapporteur")
  ) |>
  mutate(
    estimate = estimate,
    index = create_index(estimate),
    judge = term |>
      str_remove("judge_rapporteur"),
    judge = fct_reorder(judge, estimate)
  ) |>
  select(
    judge, estimate, index
  )

# GC data
gc_data <- gc_judge_means |>
  inner_join(
    gc_judge_estimates,
    by = "judge"
  ) |>
  filter(
    !is.na(index)
  )

# Calculate ranks for GC judges
gc_data <- gc_data |>
  mutate(
    judge = str_c(judge, " (", member_state_code, ")"),
    rank_average = rank(-average_duration_days),
    rank_index = rank(index)
  )

# Average duration by month plot -----------------------------------------------

# Make plot data
plot_data <- case_duration |>
  mutate(
    year = year(decision_date),
    decision_date = decision_date |>
      str_replace("-[0-9]{2}$", "-01") |>
      ymd()
  ) |>
  group_by(
    year, decision_date, court
  ) |>
  summarize(
    average_duration_days = mean(duration_days),
    .groups = "drop"
  ) |>
  filter(
    decision_date >= "2004-09-01" & decision_date <= "2021-12-31"
  ) |>
  filter(
    average_duration_days > 400
  )

# Make plot
plot <- ggplot(plot_data, aes(x = decision_date, y = average_duration_days, color = factor(court))) +
  geom_vline(xintercept = ymd("2010-03-01"), linetype = "66", linewidth = 0.5) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", formula = "y ~ x", se = FALSE) +
  scale_color_manual(values = palette_minimal(2), labels = c("Court of Justice", "General Court"), name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits = c(ymd("2004-09-01"), ymd("2021-12-31"))) +
  scale_y_continuous(breaks = seq(0, 1600, 100), expand = c(0.1, 0.1)) +
  scale_size_continuous(name = "Number of Judgments") +
  titles_minimal(
    title = "Average case duration over time by court (2004-2022)",
    y = "Average case duration (days)"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.75, 0.85),
    legend.margin = margin(t = 12, r = 12, b = 12, l = 12),
    text = element_text(family = "Roboto"),
    plot.title = element_text(family = "Roboto Medium", size = 16),
    axis.title.x = element_text(family = "Roboto Medium", size = 12),
    legend.title = element_text(family = "Roboto Medium", size = 12)
  )

# Save plot
ggsave(plot, filename = "figures/figure-1.png", device = "png", width = 10, height = 7, scale = 1.3)

# Coefficient plot -------------------------------------------------------------

# Get CJ model coefficients
cj_coefficients <- cj_model |>
  tidy() |>
  filter(
    !str_detect(term, "judge_rapporteur|Intercept")
  ) |>
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    significance = case_when(
      p.value < 0.05 ~ "Statistically significant",
      TRUE ~ "Not statistically signficiant"
    ),
    model = "Court of Justice"
  ) |>
  filter(
    !is.na(term)
  ) |>
  select(
    model, variable = term, estimate, lower, upper, significance
  )

# Get GC model coefficients
gc_coefficients <- gc_model |>
  tidy() |>
  filter(
    !str_detect(term, "judge_rapporteur|Intercept")
  ) |>
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    significance = case_when(
      p.value < 0.05 ~ "Statistically significant",
      TRUE ~ "Not statistically signficiant"
    ),
    model = "General Court"
  ) |>
  filter(
    !is.na(term)
  ) |>
  select(
    model, variable = term, estimate, lower, upper, significance
  )

# Combine models
plot_data <- bind_rows(
  cj_coefficients,
  gc_coefficients
)

# Clean variable names and code groups
plot_data <- plot_data |>
  mutate(
    group = case_when(
      variable == "count_judges" ~ "Procedural complexity",
      variable == "count_joined_proceedings" ~ "Procedural complexity",
      variable == "count_opinions" ~ "Procedural complexity",
      variable == "count_procedures" ~ "Procedural complexity",
      variable == "is_appeal" ~ "Procedural complexity",
      variable == "is_direct_action" ~ "Procedural complexity",
      variable == "count_cited_documents" ~ "Legal complexity",
      variable == "principles_of_law" ~ "Legal complexity",
      variable == "national_law" ~ "Legal complexity",
      variable == "legal_order" ~ "Legal complexity",
      variable == "cites_primary_law" ~ "Legal complexity",
      variable == "cites_secondary_law" ~ "Legal complexity",
      variable == "count_sources_of_law" ~ "Legal complexity",
      variable == "count_directory_codes" ~ "Policy complexity",
      variable == "count_subject_keywords" ~ "Policy complexity",
      variable == "count_policy_areas" ~ "Policy complexity",
      variable == "agriculture" ~ "Subject matter",
      variable == "budget" ~ "Subject matter",
      variable == "competition" ~ "Subject matter",
      variable == "consumer_protection" ~ "Subject matter",
      variable == "customs_union" ~ "Subject matter",
      variable == "economic_policy" ~ "Subject matter",
      variable == "environment" ~ "Subject matter",
      variable == "external_relations" ~ "Subject matter",
      variable == "home_affairs" ~ "Subject matter",
      variable == "industrial_policy" ~ "Subject matter",
      variable == "internal_market" ~ "Subject matter",
      variable == "justice" ~ "Subject matter",
      variable == "social_policy" ~ "Subject matter",
      variable == "taxation" ~ "Subject matter",
      variable == "trade" ~ "Subject matter",
      variable == "transport" ~ "Subject matter"
    ),
    group = factor(
      group,
      levels = c(
        "Procedural complexity", "Legal complexity",
        "Policy complexity", "Legal procedures", "Subject matter"
      )
    ),
    variable = case_when(
      variable == "count_judges" ~ "Number of judges",
      variable == "count_joined_proceedings" ~ "Number of joined proceedings",
      variable == "count_opinions" ~ "Number of AG opinions",
      variable == "count_procedures" ~ "Number of legal procedures",
      variable == "is_appeal" ~ "Appeal",
      variable == "is_direct_action" ~ "Direct action",
      variable == "count_cited_documents" ~ "Number of cited documents",
      variable == "principles_of_law" ~ "General principles of law",
      variable == "national_law" ~ "National law",
      variable == "legal_order" ~ "EU legal order",
      variable == "count_sources_of_law" ~ "Number of sources of law",
      variable == "cites_primary_law" ~ "Cites primary law",
      variable == "cites_secondary_law" ~ "Cites secondary law",
      variable == "count_directory_codes" ~ "Number of directory codes",
      variable == "count_subject_keywords" ~ "Number of subject keywords",
      variable == "count_policy_areas" ~ "Number of policy areas",
      variable == "agriculture" ~ "Agriculture",
      variable == "budget" ~ "Budget",
      variable == "competition" ~ "Competition",
      variable == "consumer_protection" ~ "Consumer protection",
      variable == "customs_union" ~ "Customs union",
      variable == "economic_policy" ~ "Economic policy",
      variable == "environment" ~ "Environment",
      variable == "external_relations" ~ "External relations",
      variable == "home_affairs" ~ "Home affairs",
      variable == "industrial_policy" ~ "Industrial policy",
      variable == "internal_market" ~ "Internal market",
      variable == "justice" ~ "Justice",
      variable == "social_policy" ~ "Social policy",
      variable == "taxation" ~ "Taxation",
      variable == "trade" ~ "Trade",
      variable == "transport" ~ "Transport"
    )
  )

# Make plot
plot <- ggplot(plot_data) +
  geom_vline(xintercept = 0, color = "gray90", linewidth = 0.5) +
  geom_point(aes(x = estimate, y = fct_rev(variable), color = significance)) +
  geom_segment(aes(x = lower, xend = upper, y = variable, yend = variable, color = significance)) +
  scale_x_continuous(n.breaks = 8) +
  scale_color_manual(values = c("#E74C3C", "#3498DB"), name = NULL) +
  facet_grid(rows = vars(group), cols = vars(model), scales = "free", space = "free_y") +
  labs(
    title = "Estimated marginal effects",
    x = "Estimated effect on case duration (days)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text("Roboto"),
    plot.title = element_text("Roboto Medium", size = 16),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank(),
    strip.text.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
    strip.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10), hjust = 0, angle = 0),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

# Save plot
ggsave(plot, file = "figures/figure-2.png", width = 10, height = 9, scale = 1.2)

# Judge plots ------------------------------------------------------------------

## CJ --------------------------------------------------------------------------

# Make plot
plot <- ggplot(cj_data) +
  geom_vline(xintercept = mean(cj_data$index), color = "gray90", linewidth = 0.5) +
  geom_point(aes(x = index, y = fct_reorder(judge, index), size = count_judgments, color = factor(is_current), fill = factor(is_current)), pch = 21, stroke = 0.3) +
  geom_point(aes(x = index, y = fct_reorder(judge, index), size = count_judgments, color = factor(is_current)), size = 1) +
  geom_text(aes(x = index + 0.25, y = fct_reorder(judge, index), label = judge), size = 2.5, color = "gray25", hjust = 0) +
  scale_color_manual(values = rev(palette_minimal(2)), labels = c("Former", "Current"), name = "Status") +
  scale_fill_manual(values = rev(alpha(palette_minimal(2), 0.2)), guide = "none") +
  scale_size_continuous(range = c(1, 8), guide = "none", breaks = seq(50, 300, 50), name = "Number of judgments") +
  scale_x_continuous(breaks = c(0, mean(cj_data$index), 10), labels = c("Less productive (0)", "Average", "More productive (10)"), expand = expansion(mult = c(0.05, 0.2))) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  titles_minimal(
    title = str_c("Estimated productivity of judges at the Court of Justice (2004-2022)"),
    x = "Productivity index"
  ) +
  theme_minimal() +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  ) +
  theme(
    legend.position = c(0.85, 0.3),
    legend.margin = margin(t = 12, r = 12, b = 12, l = 12),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(family = "Roboto"),
    plot.title = element_text(family = "Roboto Medium", size = 14),
    axis.title.x = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# Save plot
ggsave(plot, file = "figures/figure-3.png", width = 10, height = 10, scale = 1.3)

## GC --------------------------------------------------------------------------

# Make plot
plot <- ggplot(gc_data) +
  geom_vline(xintercept = mean(gc_data$index), color = "gray90", linewidth = 0.5) +
  geom_point(aes(x = index, y = fct_reorder(judge, index), size = count_judgments, color = factor(is_current), fill = factor(is_current)), pch = 21, stroke = 0.3) +
  geom_point(aes(x = index, y = fct_reorder(judge, index), size = count_judgments, color = factor(is_current)), size = 1) +
  geom_text(aes(x = index + 0.25, y = fct_reorder(judge, index), label = judge), size = 2.5, color = "gray25", hjust = 0) +
  scale_color_manual(values = rev(palette_minimal(2)), labels = c("Former", "Current"), name = "Status") +
  scale_fill_manual(values = rev(alpha(palette_minimal(2), 0.2)), guide = "none") +
  scale_size_continuous(range = c(1, 8), guide = "none", breaks = seq(50, 300, 50), name = "Number of judgments") +
  scale_x_continuous(breaks = c(0, mean(gc_data$index), 10), labels = c("Less productive (0)", "Average", "More productive (10)"), expand = expansion(mult = c(0.05, 0.2))) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  titles_minimal(
    title = str_c("Estimated productivity of judges at the General Court (2004-2022)"),
    x = "Productivity index"
  ) +
  theme_minimal() +
  guides(
    color = guide_legend(order = 1),
    size = guide_legend(order = 2)
  ) +
  theme(
    legend.position = c(0.85, 0.2),
    legend.margin = margin(t = 12, r = 12, b = 12, l = 12),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(family = "Roboto"),
    plot.title = element_text(family = "Roboto Medium", size = 14),
    axis.title.x = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# Save plot
ggsave(plot, file = "figures/figure-4.png", width = 10, height = 12, scale = 1.3)

# Rank plots -------------------------------------------------------------------

## CJ --------------------------------------------------------------------------

# Make plot data for CJ judges
plot_data <- bind_rows(
  cj_data |>
    select(
      judge, rank = rank_average, count_judgments, is_current
    ) |>
    mutate(
      metric = "Average case duration",
      x_position = 0,
      hjust = 1
    ),
  cj_data |>
    select(
      judge, rank = rank_index, count_judgments, is_current
    ) |>
    mutate(
      metric = "Productivity index",
      x_position = 1,
      hjust = 0
    )
)

# Code factor
plot_data <- plot_data |>
  mutate(
    metric = factor(metric, levels = c("Average case duration", "Productivity index"))
  )

# Make plot
plot <- ggplot(plot_data) +
  geom_line(aes(x = x_position, y = rank, group = judge), color = "gray90", linewidth = 0.5) +
  geom_point(aes(x = x_position, y = rank), size = 2, color = "white") +
  geom_point(aes(x = x_position, y = rank, color = factor(is_current), fill = factor(is_current)), size = 2, pch = 21, stroke = 0.75) +
  geom_text(aes(x = x_position + ifelse(metric == "Average case duration", -0.05, 0.05), y = rank, label = judge, hjust = hjust), color = "gray25", size = 3) +
  annotate("text", x = 0.5, y = 5, label = "Less productive", size = 4, family = "Roboto") +
  annotate("text", x = 0.5, y = 55, label = "More productive", size = 4, family = "Roboto") +
  scale_color_manual(values = c("#E74C3C", "#3498DB"), labels = c("Former", "Current"), name = NULL) +
  scale_fill_manual(values = alpha(c("#E74C3C", "#3498DB"), 0.2), labels = c("Former", "Current"), name = NULL) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Average case duration", "Productivity index"), expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(2, 1))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    title = "Court of Justice (2004-2022)"
  ) +
  theme_minimal() +
  theme(
    legend.margin = margin(t = 12, r = 12, b = 12, l = 12),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(family = "Roboto"),
    plot.title = element_text(family = "Roboto Medium", size = 16),
    axis.title = element_text(family = "Roboto", size = 12),
    legend.title = element_text(family = "Roboto", size = 12),
    plot.margin = margin(l = 100, r = 100, t = 50, b = 50),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

# Save plot
ggsave(plot, file = "figures/figure-5.png", width = 10, height = 14, scale = 1)

## GC --------------------------------------------------------------------------

# Make plot data for CJ judges
plot_data <- bind_rows(
  gc_data |>
    select(
      judge, rank = rank_average, count_judgments, is_current
    ) |>
    mutate(
      metric = "Average case duration",
      x_position = 0,
      hjust = 1
    ),
  gc_data |>
    select(
      judge, rank = rank_index, count_judgments, is_current
    ) |>
    mutate(
      metric = "Productivity index",
      x_position = 1,
      hjust = 0
    )
)

# Code factor
plot_data <- plot_data |>
  mutate(
    metric = factor(metric, levels = c("Average case duration", "Productivity index"))
  )

# Make plot
plot <- ggplot(plot_data) +
  geom_line(aes(x = x_position, y = rank, group = judge), color = "gray90", linewidth = 0.5) +
  geom_point(aes(x = x_position, y = rank), size = 2, color = "white") +
  geom_point(aes(x = x_position, y = rank, color = factor(is_current), fill = factor(is_current)), size = 2, pch = 21, stroke = 0.75) +
  geom_text(aes(x = x_position + ifelse(metric == "Average case duration", -0.05, 0.05), y = rank, label = judge, hjust = hjust), color = "gray25", size = 3) +
  annotate("text", x = 0.5, y = 5, label = "Less productive", size = 4, family = "Roboto") +
  annotate("text", x = 0.5, y = 89, label = "More productive", size = 4, family = "Roboto") +
  scale_color_manual(values = rev(palette_minimal(2)), labels = c("Former", "Current"), name = NULL) +
  scale_fill_manual(values = alpha(rev(palette_minimal(2)), 0.2), labels = c("Former", "Current"), name = NULL) +
  scale_size_continuous(range = c(1, 8), guide = "none") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Average case duration", "Productivity index"), expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(expand = expansion(add = c(2, 1))) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    title = "General Court (2004-2022)"
  ) +
  theme_minimal() +
  theme(
    legend.margin = margin(t = 12, r = 12, b = 12, l = 12),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(family = "Roboto"),
    plot.title = element_text(family = "Roboto Medium", size = 16),
    axis.title = element_text(family = "Roboto", size = 12),
    legend.title = element_text(family = "Roboto", size = 12),
    plot.margin = margin(l = 130, r = 130, t = 50, b = 50),
    legend.position = "bottom",
    legend.direction = "vertical"
  )

# Save plot
ggsave(plot, file = "figures/figure-6.png", width = 10, height = 19, scale = 1)
