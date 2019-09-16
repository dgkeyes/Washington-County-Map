# Packages ----------------------------------------------------------------

library(tidyverse)
library(tigris)
library(janitor)
library(sf)
library(shinyWidgets)
library(rmarkdown)
library(leaflet)

# Load Data ---------------------------------------------------------------

source("load-data.R")

# UI ----------------------------------------------------------------------

navbarPage("Washington County Map",
           tabPanel("All Layers",
                    h3("Notes"),
                    p("This shows all layers on one map. I don't think it's a good representation at all. It's not clear what's happening when you put several layers on top of each other, and I think people would be very confused by this. I definitely recommend scrapping this."),
                    leafletOutput("all_layers_map",
                                  height = 600)
                    
           ),
           tabPanel("Bivariate Choropleth",
                    sidebarLayout(
                      sidebarPanel(width = 3,
                        varSelectInput(inputId = "bivariate_xvar",
                                       label = "Variable #1",
                                       data = select(census_tracts_wide, `Children 5-17`:`Non-English Speakers`) %>% as.data.frame),
                        varSelectInput(inputId = "bivariate_yvar",
                                       label = "Variable #2",
                                       data = select(census_tracts_wide, `Children 5-17`:`Non-English Speakers`) %>% as.data.frame)
                        # awesomeRadio(
                        #   inputId = "bivariate_xvar",
                        #   label = "X",
                        #   choices = social_indicators_vars
                        # ),
                        # awesomeRadio(
                        #   inputId = "bivariate_yvar",
                        #   label = "Y",
                        #   choices = social_indicators_vars
                        # )
                      ),
                      
                      mainPanel(width = 9,
                                h3("Notes"),
                                p("This is the bivariate choropleth I talked about. You can select two variables and then it will make a bivariate choropleth map, which shows the interaction of these two variables. For example, in the maps, the darkest blue are census tracts with the highest two rates for both variables (e.g. children 5-17 and non-English speakers)."),
                        plotOutput("bivariate_plot",
                                   height = 600)
                      )
                      
                    )
           ),
           tabPanel("Composite Score",
                    h3("Notes"),
                    p("This map was created by making a composite score for each census tract. Please note that the scores right now are basically meaningless."),
                    p("The idea, though, would be that you would have a composite score for each tract, calculated by how high it scores on various risk factors. The challenge is how to define these composite scores."),
                    p("Here are a couple options: 1) For each measure, calculate quartiles and assign a score (1, 2, 3, 4). Then create a composite score by summing the score for each measure. 2) Create a dichotomous variable for each tract (i.e. has this risk factor/does not have this risk factor). Then, calculate composite score by how many risk factors each census tract has."),
                    leafletOutput("composite_map",
                                  height = 600)
           )
)
