#  Load packages ----------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(scales)

# Import survey data ------------------------------------------------------

bird_data <-
  read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  mutate(year = lubridate::year(date_mm_dd_yyyy),
         week = lubridate::week(date_mm_dd_yyyy),
         month = lubridate::month(date_mm_dd_yyyy, label = T),
         month_day = format(date_mm_dd_yyyy, format = "%m-%d")) %>%
  select(date_mm_dd_yyyy,
         year,
         week,
         month,
         month_day,
         everything())

# List of set species names
species_list <-
  read_csv("Data/SpeciesList.csv") %>%
  pull(SpeciesList)


# Birds found by date/year ------------------------------------------------

PlotData <-
  bird_data %>%
    select(date_mm_dd_yyyy, year, week, month, month_day) %>%
    as_tibble(.) %>%
    group_by(date_mm_dd_yyyy) %>%
    add_count() %>%
    mutate(year = as_factor(year)) %>%
    distinct()


PlotData %>%
  ggplot(., aes(x = month_day, y = n, group = year, fill = year)) +
  geom_area(alpha = 0.5) +
  geom_smooth(aes(color = year), se = FALSE) +
  scale_x_discrete(breaks = c("09-01", "10-01", "11-02"),
                   labels = c("September", "October", "November")) +
  scale_fill_fivethirtyeight() +
  scale_color_fivethirtyeight() +
  xlab("Month") +
  ylab("Number of Birds Found") +
  theme_fivethirtyeight() +
  scale_y_continuous(
    breaks = function(x) unique(
      floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 20, face = "bold"),
    text = element_text( size = 20),
    legend.position = "top",
    panel.grid.major.x = element_blank())
