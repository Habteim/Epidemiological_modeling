
# Part 1. Data import and basic manipulation 
#--------------------------------------------------------
  
  # 1. Import/load the provided CSV data into a data frame
library(dplyr)
library(tidyverse)
library(readxl)
  
setwd("/Users/apple/Disease_modeling")
vaccination_coverage <- read_excel("vaccination_coverage.xlsx")
measles_incidence <- read_excel("measles_incidence.xlsx")
reported_cases <- read_excel("reported_cases.xlsx")


View(vaccination_coverage)
head(vaccination_coverage, 10)

#Generate unique ID number
vaccination_coverage <- vaccination_coverage %>%
  mutate(ID = row_number())


library(tidyverse)
library(ggplot2)

# Read and transform data
wide_data <- read_excel("vaccination_coverage.xlsx", sheet = "Sheet1") %>%
  select(YEAR, COVERAGE_CATEGORY, COVERAGE) %>%
  pivot_wider(
    names_from = COVERAGE_CATEGORY,
    values_from = COVERAGE,
    names_glue = "{COVERAGE_CATEGORY}_COVERAGE"
  ) %>%
  filter(YEAR >= 2000)  # Keep all years since 2000

# Convert to long format for plotting
plot_data <- wide_data %>%
  pivot_longer(
    cols = ends_with("COVERAGE"),
    names_to = "COVERAGE_TYPE",
    values_to = "COVERAGE_PCT"
  ) %>%
  mutate(COVERAGE_TYPE = str_remove(COVERAGE_TYPE, "_COVERAGE"))

#=======================================================================
#Plot of the national vaccination coverage 

ggplot(wide_data) +
  geom_line(aes(x=YEAR, y=ADMIN_COVERAGE, color="Administrative")) +
  geom_line(aes(x=YEAR, y=OFFICIAL_COVERAGE, color="Official")) +
  geom_line(aes(x=YEAR, y=WUENIC_COVERAGE, color="WUENIC")) +
  scale_color_manual(name="Coverage Type",
                     values=c("Administrative"="blue",
                              "Official"="orange",
                              "WUENIC"="green"))
labs(
title = "Measles Vaccine (MCV1) Coverage Trends (2000–2023)",
subtitle = "Comparing Administrative, Official, and WUENIC Estimates",
x = "Year",
y = "Coverage (%)",
color = "Data Source")

ggplot(wide_data) +
  geom_line(aes(x = YEAR, y = ADMIN_COVERAGE, color = "Administrative")) +
  geom_line(aes(x = YEAR, y = OFFICIAL_COVERAGE, color = "Official")) +
  geom_line(aes(x = YEAR, y = WUENIC_COVERAGE, color = "WUENIC")) +
  scale_color_manual(name = "Coverage Type",
                     values = c("Administrative" = "blue",
                                "Official" = "orange",
                                "WUENIC" = "green")) +
  labs(
    title = "Measles Vaccine (MCV1) Coverage Trends (2000–2023)",
    subtitle = "Comparing Administrative, Official, and WUENIC Estimates",
    x = "Year",
    y = "Coverage (%)",
    color = "Data Source"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




#=======================================================================

ggplot(plot_data, aes(x = YEAR, y = COVERAGE_PCT, color = COVERAGE_TYPE)) +
  geom_line(linewidth = 1.2) +  # Thicker lines
  geom_point(size = 2.5) +      # Larger points
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +  # Labels every 2 years
  labs(
    title = "Measles Vaccine (MCV1) Coverage Trends (2000–2023)",
    subtitle = "Comparing Administrative, Official, and WUENIC Estimates",
    x = "Year",
    y = "Coverage (%)",
    color = "Data Source"
  ) +
  scale_color_manual(
    values = c("ADMIN" = "#1f77b4", "OFFICIAL" = "#ff7f0e", "WUENIC" = "#2ca02c"),
    labels = c("Administrative", "Official", "WHO/UNICEF (WUENIC)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 100)  # Set y-axis limits


#Faceted plot
ggplot(plot_data, aes(x = YEAR, y = COVERAGE_PCT)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~COVERAGE_TYPE, ncol = 1, 
             labeller = as_labeller(c(
               "ADMIN" = "Administrative Coverage",
               "OFFICIAL" = "Official Coverage",
               "WUENIC" = "WHO/UNICEF (WUENIC)"))) +
  labs(title = "MCV1 Coverage Trends by Data Source (2000–2023)")



# Measles incidence per 1 million population in Ethiopia 2000-2023
# Read and clean the data
measles_data <- read_excel("measles_incidence.xlsx", sheet = "Sheet1") %>%
  # Filter for Ethiopia and remove metadata rows
  filter(`Country / Region` == "Ethiopia", !is.na(Disease)) %>%
  # Select year columns and convert to long format
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
  arrange(YEAR)  # Sort chronologically

#Plot the data on line graph

ggplot(measles_data, aes(x = YEAR, y = INCIDENCE_RATE)) +
  geom_line(color = "#e41a1c", linewidth = 1.2) +  # Red line for visibility
  geom_point(color = "#e41a1c", size = 2.5) +      # Red points
  scale_x_continuous(
    breaks = seq(2000, 2023),  # Labels every 2 years
    limits = c(2000, 2023)             # Force full time range
  ) +
  labs(
    title = "Measles Incidence in Ethiopia (2000–2023)",
    subtitle = "Reported cases per 1,000,000 population",
    x = "Year",
    y = "Incidence Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )


ggsave("Trend of measles cases in Ethiopia 2000-2023.png", width = 12, height = 8, dpi = 300)


#Highlighting recent outbreaks

measles_data %>%
  filter(YEAR >= 2012) %>%
  ggplot(aes(x = factor(YEAR), y = INCIDENCE_RATE)) +
  geom_col(fill = "#e41a1c", width = 0.7) +
  geom_text(
    aes(label = round(INCIDENCE_RATE, 1)),
    vjust = -0.5,
    color = "black",
    size = 3.5
  ) +
  labs(title = "Earlier and recent Measles Resurgence in Ethiopia (2020–2023)")

