#Script to combine the measles incidence and highlighting recent outbreaks

library(tidyverse)

measles_incidence <- read_excel("measles_incidence.xlsx", sheet = "Sheet1")

# Clean and transform measles incidence data
measles_data <- measles_incidence %>%
  filter(`Country / Region` == "Ethiopia", !is.na(Disease)) %>%
  select(-c(`Country / Region`, Disease, Denominator)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "YEAR",
    values_to = "INCIDENCE_RATE"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    INCIDENCE_RATE = as.numeric(INCIDENCE_RATE)
  ) %>%
  arrange(YEAR)

# Prepare data for line plot
trend_data <- measles_data %>%
  mutate(Plot_Type = "Trend (2000–2023)")

# Prepare data for bar plot
recent_data <- measles_data %>%
  filter(YEAR >= 2012) %>%
  mutate(Plot_Type = "Recent Resurgence (2012–2023)")


library(tidyverse)
library(patchwork)

# --- Line Plot: 2000–2023 ---
p1 <- ggplot(measles_data, aes(x = YEAR, y = INCIDENCE_RATE)) +
  geom_line(color = "#e41a1c", linewidth = 1.2) +
  geom_point(color = "#e41a1c", size = 2.5) +
  scale_x_continuous(breaks = 2000:2023, limits = c(2000, 2023)) +
  labs(
    title = "Measles Incidence in Ethiopia (2000–2023)",
    y = "Incidence Rate per 1M",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# --- Bar Plot: 2012–2023 but with empty bars from 2000–2011 ---
bar_data <- tibble(YEAR = 2000:2023) %>%
  left_join(measles_data, by = "YEAR") %>%
  mutate(INCIDENCE_RATE = ifelse(YEAR < 2012, NA, INCIDENCE_RATE))

p2 <- ggplot(bar_data, aes(x = YEAR, y = INCIDENCE_RATE)) +
  geom_col(fill = "#e41a1c", width = 0.7) +
  geom_text(
    aes(label = ifelse(!is.na(INCIDENCE_RATE), round(INCIDENCE_RATE, 1), "")),
    vjust = -0.5, size = 3.2, color = "black"
  ) +
  scale_x_continuous(breaks = 2000:2023, limits = c(2000, 2023)) +
  labs(
    title = "Measles Resurgence in Ethiopia (Bar Plot: 2012–2023)",
    x = "Year",
    y = "Incidence Rate per 1M"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# --- Combine with aligned x-axis ---
combined_plot <- p1 / p2 + plot_layout(heights = c(2, 1))

# Display
combined_plot
