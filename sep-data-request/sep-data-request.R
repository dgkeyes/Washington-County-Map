
# Packages ----------------------------------------------------------------

library(dkmisc)
library(tidycensus)
library(janitor)
library(tidyverse)
library(sf)
library(tigris)
library(hrbrthemes)
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


washington_county_census_tracts <- tracts(state = "OR",
                                          county = "Washington") %>% 
  clean_names()



# Theme -------------------------------------------------------------------

theme_set(theme_ipsum(grid_col = "transparent",
                      base_family = "Inter"))

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
  left_join(under_5_by_census_tract, by = "geoid") %>% 
  mutate(pct = race_ethnicity_group / total)


# Total Count of Children Under 5 -----------------------------------------

children_under_5 <- left_join(under_5_by_census_tract,
                              population_by_census_tract,
                              by = "geoid") %>% 
  set_names(c("geoid", "under_5", "total")) %>% 
  mutate(pct = under_5 / total) %>% 
  select(geoid, pct) %>% 
  right_join(washington_county_census_tracts) %>% 
  st_as_sf()

dk_make_static_map <- function(df, title, caption, upper_limit, step) {
  
  ggplot(df) +
    geom_sf(aes(fill = pct),
            color = "transparent") +
    scale_fill_viridis_c(option = "D",
                         label = percent_format(1),
                         limits = c(0, upper_limit),
                         breaks = seq(0, .15, by = step)) +
    labs(fill = NULL,
         title = title,
         caption = caption) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
}


dk_make_static_map(children_under_5, "Children Under 15", "ACS 2017", .15, .05)


ggplot(children_under_5) +
  geom_sf(aes(fill = pct),
          color = "transparent") +
  scale_fill_viridis_c(option = "D",
                       label = percent_format(1),
                       limits = c(0, .15),
                       breaks = c(0, .05, .1, .15)) +
  labs(fill = NULL,
       title = "Children Under 5") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  

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
  select(geoid, pct) 


# Household Status: Single Parent -----------------------------------------

single_parent_families <- get_acs(geography = "tract",
                                  year = 2017,
                                  survey = "acs5",
                                  state = "OR",
                                  county = "Washington",
                                  variables = "B11014_008") %>% 
  clean_names()

# Household Status: Separted with Children --------------------------------


# Household Status: Primary Caregiver Other Than Birth Parent -------------


# Household Status: Number of Children in Home ----------------------------


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






# Household Income --------------------------------------------------------

median_income_children_under_18 <- get_acs(geography = "tract",
                                           year = 2017,
                                           survey = "acs5",
                                           state = "OR",
                                           county = "Washington",
                                           variables = (median_income = "B19125_001")) %>% 
  clean_names() %>% 
  select(geoid, estimate) 


# Foreign Born ------------------------------------------------------------

foreign_born_population <- get_acs()


