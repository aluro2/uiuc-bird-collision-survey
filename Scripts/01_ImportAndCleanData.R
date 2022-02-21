
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(janitor)


# Survey data -------------------------------------------------------------

# Ignore OAuth Token (data is publicly available)
gs4_deauth()


# URL paths to datasets
AllYearsURLs <-
  list(Fall2019 = "https://docs.google.com/spreadsheets/d/1wWO1B0BtChBz9Zjsd_22D01jRpxyTqEbpooj96954hk/edit?usp=sharing",
       Fall2020 = "https://docs.google.com/spreadsheets/d/1w362vrtdaJ7JNabmuOll_xJyPIlLjYjqO0l2DxjEDtQ/edit?usp=sharing",
       Fall2021 = "https://docs.google.com/spreadsheets/d/17K3BLBFHDZ7HP6Fh1ummLdCKNtyWFvFw0rRNQ4E6E7c/edit?usp=sharing")

SurveyDataAllYears <-
  # Import individual data sheets (map applies ~function() to each list item of AllYearsURLs)
  purrr::map(AllYearsURLs,
             ~googlesheets4::read_sheet(.x)) %>%
    # Clean up variable names
    purrr::map(., ~clean_names(.x)) %>%
    # Format collection date (ymd)
    purrr::map(., ~mutate(.x, date_mm_dd_yyyy = lubridate::as_date(date_mm_dd_yyyy))) %>%
    # Keep relevant variables
    purrr::map(., ~select(.x,-time_of_collection_hh_mm_24_hour_clock)) %>%
    purrr::map(., ~select(.x, date_mm_dd_yyyy:found_dead_alive)) %>%
    # Combine into a single dataframe
    bind_rows() %>%
    # Keep rows where birds were found
    filter(any_birds_found_on_route_shift == "Yes")


# Create a local copy, save into Data directory ---------------------------

# Save as .csv file
write_csv(SurveyDataAllYears, file = "Data/UIUC_bird_survey_data_all_years.csv")
