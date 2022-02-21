
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggthemes)


# Import data ---------------------------------------------------------------

# List of set species names
species_list <-
  read_csv("Data/SpeciesList.csv") %>%
  pull(SpeciesList)

# Import bird data, keep only obs. with positive species ID
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
  ) %>%
  filter(species %in% species_list) %>%
  filter(!species == "Unidentifiable") %>%
  filter(!species == is.na(species))


# species Plot ------------------------------------------------------------

species_plot <-
  bird_data %>%
  select(species) %>%
  # Remove scientific names
  mutate(species = str_trim(species, side = "both"),
         species = str_extract(species, "[^_]+")) %>%
  # Get count of number of individuals per species collected
  group_by(species) %>%
  add_count() %>%
  distinct() %>%
  # Sort by highest number of individuals per species
  arrange(., desc(n), species) %>%
  ungroup() %>%
  mutate(species= forcats::fct_reorder(species, desc(n)),
         totalbirds = sum(n),
         pct = round(n/totalbirds*100, 1)) %>%
  ggplot() +
  labs(x = "Species",
       y = "% of Total Birds Found",
       title = "UIUC campus bird glass collision survey",
       subtitle = "Fall migration 2019-2021") +
  # Lollipop stick
  geom_linerange(
    aes(x = species, ymin = 0, ymax = pct),
    color = "darkgray",
    size = 1.5) +
  # Lollipop candy
  geom_point(aes(fill = species, x = species, y = pct),
             color = "darkgrey",  size = 6, shape = 21 ) +
  scale_fill_viridis_d(direction = -1, option = "plasma") +
  theme_fivethirtyeight() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    text = element_text(size = 20),
    legend.position = "none",
    panel.grid.major.x = element_blank())

ggsave(filename = "Figures/PercentOfBirdsFoundBySpecies.png",
       width = 12,
       height = 8,
       units = "in")
