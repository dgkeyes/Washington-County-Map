

# Packages ----------------------------------------------------------------

library(tigris)
library(tidyverse)
library(janitor)

# Load Data ---------------------------------------------------------------

options(tigris_use_cache = TRUE,
        tigris_class = "sf")


social_indicators <- read_csv("data/social-indicators.csv") %>% 
  mutate(geoid = as.character(geoid))

social_indicators_wide <- read_csv("data/social-indicators-wide.csv") %>% 
  mutate(geoid = as.character(geoid))

social_indicators_vars <- social_indicators_wide %>% 
  select(-geoid) %>% 
  names()


census_tracts_wide <- tracts(state = "OR",
                        county = "Washington",
                        cb = TRUE) %>% 
  clean_names() %>% 
  right_join(social_indicators_wide, by = "geoid")


census_tracts_long <- tracts(state = "OR",
                             county = "Washington",
                             cb = TRUE) %>% 
  clean_names() %>% 
  right_join(social_indicators, by = "geoid")

composite_score <- tracts(state = "OR",
                          county = "Washington",
                          cb = TRUE) %>% 
  clean_names() %>% 
  right_join(social_indicators, by = "geoid") %>% 
  group_by(measure) %>% 
  mutate(quartile = ntile(value, 4)) %>% 
  ungroup() %>% 
  group_by(geoid) %>% 
  summarize(total_score = sum(quartile))
