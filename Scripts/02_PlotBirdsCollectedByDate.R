#  Load packages ----------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(scales)

# Import survey data ------------------------------------------------------

bird_data <-
  # Import data
  read_csv("Data/UIUC_bird_survey_data_all_years.csv") %>%
  # Create time variables
  mutate(
    year = lubridate::year(date_mm_dd_yyyy),
    week = lubridate::week(date_mm_dd_yyyy),
    month = lubridate::month(date_mm_dd_yyyy, label = T),
    month_day = format(date_mm_dd_yyyy, format = "%m-%d")
  ) %>%
  # Arrange variables
  select(
    date_mm_dd_yyyy,
    year,
    week,
    month,
    month_day,
    everything()
  )

# Birds found by date/year ------------------------------------------------

PlotData <-
  bird_data %>%
  # Keep time vars (rows = single observations)
  select(date_mm_dd_yyyy, year, week, month, month_day) %>%
  as_tibble(.) %>%
  # Create variable of sum of birds found on given date
  group_by(date_mm_dd_yyyy) %>%
  add_count() %>%
  # Set year as a factor (not a number)
  mutate(year = as_factor(year)) %>%
  # Get rid of duplicates
  distinct()


# Plot number of birds found by month/year --------------------------------

PlotData %>%
  ggplot(., aes(x = month_day, y = n, group = year, fill = year)) +
  # Area plot in background
  geom_area(alpha = 0.5) +
  # Loess fit lit in foreground
  geom_smooth(aes(color = year), se = FALSE) +
  # Add relevant months as x-axis labels
  scale_x_discrete(
    breaks = c("09-01", "10-01", "11-02"),
    labels = c("September", "October", "November")
  ) +
  # Use fivethirtyeight color schemes
  scale_fill_fivethirtyeight() +
  scale_color_fivethirtyeight() +
  xlab("Month") +
  ylab("Number of Birds Found") +
  # Use fivethirtyeight theme
  theme_fivethirtyeight() +
  # Adjust text angles/sizes
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 20, face = "bold"),
    text = element_text(size = 20),
    legend.position = "top",
    # remove grid for x axis
    panel.grid.major.x = element_blank()
  )


# Save plot ---------------------------------------------------------------

# Save as .png image
ggsave(
  filename = "Figures/NumberOfBirdsFoundByMonthYear.png",
  # use most current plot
  plot = last_plot(),
  # set plot size in inches
  width = 10,
  height = 8,
  units = "in"
)
