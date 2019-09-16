
# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(janitor)
library(DT)
library(scales)


# Census Variables --------------------------------------------------------

v17 <- load_variables(2017, "acs5", cache = TRUE)

v17 %>% 
  datatable()


# General Data ------------------------------------------------------------

population_by_census_tract <- get_acs(geography = "tract",
                                      year = 2017,
                                      survey = "acs5",
                                      state = "OR",
                                      county = "Washington",
                                      variables = "B01003_001") %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(total = sum(estimate)) 

under_5_by_census_tract <- get_acs(geography = "tract",
                                   year = 2017,
                                   survey = "acs5",
                                   state = "OR",
                                   county = "Washington",
                                   variables = c(male = "B01001_003",
                                                 female = "B01001_027")) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(total = sum(estimate)) 


# Race/Ethnicity ----------------------------------------------------------

# Only include child population under 5

race_ethnicity <- get_acs(geography = "tract",
                          year = 2017,
                          survey = "acs5",
                          state = "OR",
                          county = "Washington",
                          variables = c(White = "B01001A_003",
                                        White = "B01001A_018",
                                        `African American` = "B01001B_003",
                                        `African American` = "B01001B_018",
                                        `American Indian and Alaska Native` = "B01001C_003",
                                        `American Indian and Alaska Native` = "B01001C_018",
                                        Asian = "B01001D_003",
                                        Asian = "B01001D_018",
                                        `Native Hawaiian and Other Pacific Islander` = "B01001E_003",
                                        `Native Hawaiian and Other Pacific Islander` = "B01001E_018",
                                        `Other Race` = "B01001F_003",
                                        `Other Race` = "B01001F_018",
                                        `Two or More Races` = "B01001G_003",
                                        `Two or More Races` = "B01001G_018",
                                        `Hispanic or Latino` = "B01001I_003",
                                        `Hispanic or Latino` = "B01001I_018")) %>% 
  clean_names() %>% 
  group_by(geoid, variable) %>% 
  summarize(race_ethnicity_group = sum(estimate, na.rm = TRUE)) %>% 
  left_join(under_5_by_census_tract, by = "geoid") %>% 
  mutate(pct = race_ethnicity_group / total) %>% 
  mutate(quartile = ntile(pct, 4))

race_ethnicity %>% 
  select(geoid, variable, pct, quartile) %>% 
  mutate()
  spread(variable, quartile)



# Median Income -----------------------------------------------------------

median_income_children_under_18 <- get_acs(geography = "tract",
                                           year = 2017,
                                           survey = "acs5",
                                           state = "OR",
                                           county = "Washington",
                                           variables = (median_income = "B19125_001")) %>% 
    clean_names() %>% 
    select(geoid, estimate) %>% 
    rename("Median Income for Children Under 18" = "estimate")
    # mutate(plot_label = dollar(estimate)) %>% 
    # mutate(measure = "Median Income") %>% 
    # select(measure, geoid, estimate, plot_label)


# Home/primary language ---------------------------------------------------

speak_english_less_than_well_vars <- v17 %>% 
  filter(str_detect(concept, "LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER")) %>% 
  filter(str_detect(label, "less than very well")) %>% 
  pull(name)

speak_english_less_than_well <- get_acs(geography = "tract",
                                        year = 2017,
                                        survey = "acs5",
                                        state = "OR",
                                        county = "Washington",
                                        variables = speak_english_less_than_well_vars) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(limited_english_speakers = sum(estimate, na.rm = TRUE)) %>% 
  left_join(population_by_census_tract, by = "geoid") %>% 
  mutate(pct = limited_english_speakers / total) %>% 
  select(geoid, total) %>% 
  rename("Non-English Speakers" = "total")


# Total Count of Children Under 5 -----------------------------------------

children_under_5 <- left_join(under_5_by_census_tract,
                              population_by_census_tract,
                              by = "geoid") %>% 
  set_names(c("geoid", "under_5", "total")) %>% 
  mutate(pct = under_5 / total) %>% 
  select(geoid, pct) %>% 
  rename("Children Under 5" = "pct")


# Total Count of Children 5-17 --------------------------------------------

children_5_to_17 <- get_acs(geography = "tract",
                            year = 2017,
                            survey = "acs5",
                            state = "OR",
                            county = "Washington",
                            variables = c(male = "B05003_003",
                                          female = "B05003_014")) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(under_18 = sum(estimate, na.rm = TRUE)) %>% 
  left_join(under_5_by_census_tract, by = "geoid") %>% 
  rename("under_5" = "total") %>% 
  mutate(total_5_to_17 = under_18 - under_5) %>% 
  select(-c(under_18, under_5)) %>% 
  left_join(population_by_census_tract, by = "geoid") %>% 
  mutate(pct = total_5_to_17 / total) %>% 
  select(geoid, pct) %>% 
  rename("Children 5-17" = "pct")


# Merge All ---------------------------------------------------------------

dk_set_names <- function(.data) {
  
  measure_name <- .data %>% 
    select(-geoid) %>% 
    names()
  
  .data %>% 
    set_names("geoid", "value") %>% 
    mutate(measure = measure_name)
}


social_indicators <- bind_rows(dk_set_names(children_5_to_17),
                               dk_set_names(children_under_5),
                               dk_set_names(median_income_children_under_18),
                               dk_set_names(speak_english_less_than_well))


social_indicators_wide <- left_join(children_5_to_17,
                               children_under_5) %>% 
  left_join(median_income_children_under_18) %>% 
  left_join(speak_english_less_than_well)


# Write to File -----------------------------------------------------------



write_csv(social_indicators, "data/social-indicators.csv")

write_csv(social_indicators_wide, "data/social-indicators-wide.csv")

