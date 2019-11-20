
# Packages ----------------------------------------------------------------

library(dkmisc)
library(tidycensus)
library(janitor)
library(tidyverse)
library(sf)
library(tigris)
library(scales)

# Functions ---------------------------------------------------------------

dk_view_2017_acs_vars()

dk_view_2010_census_vars()

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

under_18_by_census_tract <- get_acs(geography = "tract",
                                    year = 2017,
                                    survey = "acs5",
                                    state = "OR",
                                    county = "Washington",
                                    variables = c(male_under_5 = "B01001_003",
                                                  male_5_to_9 = "B01001_004",
                                                  male_10_to_14 = "B01001_005",
                                                  male_15_to_17 = "B01001_006",
                                                  female_under_5 = "B01001_027",
                                                  female_5_to_9 = "B01001_028",
                                                  female_10_to_14 = "B01001_029",
                                                  female_15_to_17 = "B01001_030")) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(total = sum(estimate)) 


population_by_census_tract_groups <- population_by_census_tract %>% 
  left_join(under_5_by_census_tract, by = "geoid") %>% 
  left_join(under_18_by_census_tract, by = "geoid") %>% 
  set_names(c("Census Tract", "Total Population", "Under 5 Population", "Under 18 Population"))

population_by_census_tract_groups %>% 
  write_csv("sep-data-request/populations-total-under-5-under-18.csv")


# Race/Ethnicity for Children Under 5 -------------------------------------

race_ethnicity_under_5 <- get_acs(geography = "tract",
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
  pivot_wider(names_from = variable, 
              values_from = race_ethnicity_group) 

race_ethnicity_under_5 %>% 
  rename(`Census Tract` = geoid) %>% 
  write_csv("sep-data-request/race-ethnicity-under-5.csv")


# Race/Ethnicity for Children Under 18 -------------------------------------

race_ethnicity_under_18 <- get_acs(geography = "tract",
                                   year = 2017,
                                   survey = "acs5",
                                   state = "OR",
                                   county = "Washington",
                                   variables = c(White = "B01001A_003",
                                                 White = "B01001A_004",
                                                 White = "B01001A_005",
                                                 White = "B01001A_006",
                                                 White = "B01001A_018",
                                                 White = "B01001A_019",
                                                 White = "B01001A_020",
                                                 White = "B01001A_021",
                                                 `African American` = "B01001B_003",
                                                 `African American` = "B01001B_004",
                                                 `African American` = "B01001B_005",
                                                 `African American` = "B01001B_006",
                                                 `African American` = "B01001B_018",
                                                 `African American` = "B01001B_019",
                                                 `African American` = "B01001B_020",
                                                 `African American` = "B01001B_021",
                                                 `American Indian and Alaska Native` = "B01001C_003",
                                                 `American Indian and Alaska Native` = "B01001C_004",
                                                 `American Indian and Alaska Native` = "B01001C_005",
                                                 `American Indian and Alaska Native` = "B01001C_006",
                                                 `American Indian and Alaska Native` = "B01001C_018",
                                                 `American Indian and Alaska Native` = "B01001C_019",
                                                 `American Indian and Alaska Native` = "B01001C_020",
                                                 `American Indian and Alaska Native` = "B01001C_021",
                                                 Asian = "B01001D_003",
                                                 Asian = "B01001D_004",
                                                 Asian = "B01001D_005",
                                                 Asian = "B01001D_006",
                                                 Asian = "B01001D_018",
                                                 Asian = "B01001D_019",
                                                 Asian = "B01001D_020",
                                                 Asian = "B01001D_021",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_003",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_004",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_005",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_006",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_018",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_019",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_020",
                                                 `Native Hawaiian and Other Pacific Islander` = "B01001E_021",
                                                 `Other Race` = "B01001F_003",
                                                 `Other Race` = "B01001F_004",
                                                 `Other Race` = "B01001F_005",
                                                 `Other Race` = "B01001F_006",
                                                 `Other Race` = "B01001F_018",
                                                 `Other Race` = "B01001F_019",
                                                 `Other Race` = "B01001F_020",
                                                 `Other Race` = "B01001F_021",
                                                 `Two or More Races` = "B01001G_003",
                                                 `Two or More Races` = "B01001G_004",
                                                 `Two or More Races` = "B01001G_005",
                                                 `Two or More Races` = "B01001G_006",
                                                 `Two or More Races` = "B01001G_018",
                                                 `Two or More Races` = "B01001G_019",
                                                 `Two or More Races` = "B01001G_020",
                                                 `Two or More Races` = "B01001G_021",
                                                 `Hispanic or Latino` = "B01001I_003",
                                                 `Hispanic or Latino` = "B01001I_004",
                                                 `Hispanic or Latino` = "B01001I_005",
                                                 `Hispanic or Latino` = "B01001I_006",
                                                 `Hispanic or Latino` = "B01001I_018",
                                                 `Hispanic or Latino` = "B01001I_019",
                                                 `Hispanic or Latino` = "B01001I_020",
                                                 `Hispanic or Latino` = "B01001I_021")) %>%
  clean_names() %>% 
  group_by(geoid, variable) %>% 
  summarize(race_ethnicity_group = sum(estimate, na.rm = TRUE)) %>% 
  pivot_wider(names_from = variable, 
              values_from = race_ethnicity_group)  

race_ethnicity_under_18 %>% 
  rename(`Census Tract` = geoid) %>% 
  write_csv("sep-data-request/race-ethnicity-under-18.csv")

# Total Count of Children Under 5 -----------------------------------------

under_5_by_census_tract %>% 
  set_names(c("Census Tract", "Under 5 Population")) %>% 
  write_csv("sep-data-request/children-under-5.csv")

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
  left_join(population_by_census_tract, by = "geoid")

children_5_to_17 %>% 
  select(geoid, total_5_to_17) %>% 
  set_names(c("Census Tract", "Children 5-17")) %>% 
  write_csv("sep-data-request/children-5-17.csv")


# Household Status: Single Parent -----------------------------------------

single_parent_families <- get_acs(geography = "tract",
                                  year = 2017,
                                  survey = "acs5",
                                  state = "OR",
                                  county = "Washington",
                                  variables = c(under_6 = "B23008_008",
                                                age_6_17 = "B23008_021")) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(children_living_with_single_parent = sum(estimate))

single_parent_families %>% 
  set_names("Census Tract", "Children Living with Single Parent") %>% 
  write_csv("sep-data-request/children-living-with-single-parent.csv")

# Household Status: Separated with Children --------------------------------

# Not available

# Household Status: Primary Caregiver Other Than Birth Parent -------------

# Not available

# Household Status: Number of Children in Home ----------------------------

# I think it comes from here https://www.census.gov/programs-surveys/cps/data-detail.html

# Household Status: Grandparents Raising Grandchildren --------------------------------------

grandparents_raising_grandchildren <- get_acs(geography = "tract",
                                              year = 2017,
                                              survey = "acs5",
                                              state = "OR",
                                              county = "Washington",
                                              variables = "B99103_002") %>% 
  clean_names() %>% 
  left_join(under_18_by_census_tract, by = "geoid") %>% 
  mutate(pct = estimate / total)



# Median Household Income --------------------------------------------------------

median_income_children_under_18 <- get_acs(geography = "tract",
                                           year = 2017,
                                           survey = "acs5",
                                           state = "OR",
                                           county = "Washington",
                                           variables = (median_income = "B19125_001")) %>% 
  clean_names() %>% 
  select(geoid, estimate) 

median_income_children_under_18 %>% 
  set_names("Census Tract", "Median Income for Households with Children Under 18") %>% 
  write_csv("sep-data-request/median-income-households-with-children-under-18.csv")


# Public Assistance -------------------------------------------------------

public_assistance_children_under_18_by_tract <- get_acs(geography = "tract",
                                                        year = 2017,
                                                        survey = "acs5",
                                                        state = "OR",
                                                        county = "Washington",
                                                        summary_var = "B09010_001",
                                                        variables = "B09010_002") %>%
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(received_assistance = sum(estimate))


public_assistance_children_under_18_by_tract %>% 
  set_names(c("Census Tract", "Children Under 18 Who Received Public Assistance (Suplemental Security Income, Cash Assistance, or Food Stamps/SNAP)")) %>% 
  write_csv("sep-data-request/public-assistance-under-18-by-census-tract.csv")

public_assistance_children_under_18_by_county <- get_acs(geography = "county",
                                               year = 2017,
                                               survey = "acs5",
                                               state = "OR",
                                               county = "Washington",
                                               summary_var = "B09010_001",
                                               variables = "B09010_002") %>%
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(received_assistance = sum(estimate))


public_assistance_children_under_18 %>% 
  select(received_assistance) %>% 
  set_names("Children Under 18 Who Received Public Assistance (Suplemental Security Income, Cash Assistance, or Food Stamps/SNAP)") %>% 
  write_csv("sep-data-request/public-assistance-under-18-by-county.csv")

# Foreign Born Under 18 ------------------------------------------------------------

foreign_born_population_under_18 <- get_acs(geography = "tract",
                                            year = 2017,
                                            survey = "acs5",
                                            state = "OR",
                                            county = "Washington",
                                            variables = c(male_foreign_born_under_18 = "B05003_005",
                                                          female_foreign_born_under_18 = "B05003_016")) %>% 
  clean_names() %>% 
  group_by(geoid) %>% 
  summarize(foreign_born = sum(estimate)) %>% 
  select(geoid, foreign_born) %>% 
  left_join(under_18_by_census_tract, by = "geoid") 

foreign_born_population_under_18 %>% 
  select(-total) %>% 
  set_names("Census Tract", "Foreign-Born Population Under 18") %>% 
  write_csv("sep-data-request/foreign-born-population-under-18.csv")

