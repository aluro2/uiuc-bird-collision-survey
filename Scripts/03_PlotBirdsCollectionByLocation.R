
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggmap)
library(ggrepel)


# Import data -------------------------------------------------------------

bird_data <-
  # Import data
  read_csv("Data/UIUC_bird_survey_data_all_years.csv")

building_locations  <-
  read_csv("Data/campus_building_locations.csv")

UIUC <-
  readRDS("Data/UIUC_map.rds")


# Get number of bird collisions by location -------------------------------

bird_data_map <-
  bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes") %>%
  left_join(., building_locations, by = "building_name") %>%
  select(building_name, lat, lon) %>%
  group_by(building_name, lat, lon) %>%
  summarise(n_birds = n()) %>%
  # Keep only obs where > 30 birds collected
  filter(n_birds > 30)


# Plot map of bird collisions at UIUC ------------------------------------------------

## Map with select building labels (> 30 birds found)
# Use campus map
ggmap(UIUC) +
  geom_point(data = bird_data_map,
             aes(x = lon,
                 y = lat,
                 size = n_birds,
                 fill = n_birds),
             shape = 21,
             stroke = 3,
             alpha = 0.95) +
  # Set color gradient scale
  scale_fill_gradient2(low = "blue",
                       mid = "red",
                       high = "yellow",
                       midpoint = 125,
                       breaks = seq(0, 300, 50),
                       labels = seq(0, 300, 50)) +
  # Titles
  labs(fill = "Number of \n Window-Killed Birds",
       subtitle = "Survey conducted during fall migration periods from 2019-2021") +
  # Label data points with building names
  geom_label_repel(data = bird_data_map,
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force = 25,
                   label.size = 0.01,
                   nudge_x = -1,
                   direction = "y",
                   hjust = 0,
                   segment.size = 1,
                   segment.color = "black",
                   fill = "black",
                   color = "white",
                   alpha = 1,
                   size = 7) +
  # Get rid of legend for size, customize appearance of fill color legend
  guides(size = "none",
         fill = guide_colorbar(barwidth = 2,
                               barheight = 10,
                               #label.position = "left",
                               ticks.colour = "black",
                               ticks.linewidth = 2,
                               direction = "vertical",
                               frame.colour = "black",
                               frame.linewidth = 3,
                               label.theme = element_text(colour = "white",
                                                          size = 18,
                                                          angle = 0))) +
  scale_size_area(max_size = 20) +
  # Zoom in view
  coord_cartesian(expand = TRUE) +
  theme_void() +
  theme(
    text = element_text(size = 24),
    legend.title = element_text(color = "white"),
    # Legend Title center-align
    legend.title.align = 0.5,
    # Change legend background color
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(1, "in"),
    legend.key.width = unit(1.25,"in"),
    legend.margin = margin(t = 0.5, b = 0.25, l = 0.5, r = 0.5, unit = "in"),
    # Change Legend position
    legend.position = c(0.85,0.75),
    plot.subtitle = element_text(size = 20, face = "italic", hjust =, 0.5),
    panel.background = element_rect(fill = "black", colour = NA),
    # color behind plot
    plot.background = element_rect(fill = "white")
  )

ggsave("Figures/NumberOfBirdsByLocation.png",
       dpi = 220,
       height = 9,
       width = 13,
       units = "in")
