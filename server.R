
# Packages ----------------------------------------------------------------

library(tidyverse)
library(tigris)
library(janitor)
library(sf)
library(shinyjs)
library(leaflet)
library(biscale)
library(rlang)
library(cowplot)


# Server ------------------------------------------------------------------

source("load-data.R")



function(input, output, session) {

  biscale_data <- reactive({
    census_tracts_wide %>% 
      bi_class(x = !!input$bivariate_xvar,
               y = !!input$bivariate_yvar,
               style = "quantile",
               dim = 3)
  })

  output$bivariate_plot <- renderPlot({
    
 
    
    
    map <- ggplot() +
      geom_sf(data = biscale_data(), 
              mapping = aes(fill = bi_class), 
              color = "white", 
              size = 0.1, 
              show.legend = FALSE) +
      labs(title = str_wrap(str_glue("{as.character(input$bivariate_xvar)} and {as.character(input$bivariate_yvar)}"), 40),
           subtitle = "Washington County, Oregon") +
      bi_scale_fill(pal = "DkBlue", dim = 3) +
      bi_theme() 
    
    legend <- bi_legend(pal = "DkBlue",
                        dim = 3,
                        xlab = as.character(input$bivariate_xvar),
                        ylab = as.character(input$bivariate_yvar),
                        size = 10)
    
    ggdraw() +
      draw_plot(map, 0, 0, 1, 1) +
      draw_plot(legend, 0, 0, 0.3, 0.3) 
  })


  dk_add_layer <- function(.data, measure_name) {
    .data %>% 
      addPolygons(data = filter(census_tracts_long, measure == measure_name), 
                  group = measure_name,
                  fillColor = ~colorNumeric("Blues", value,
                                            na.color = "#eeeeee")(value),
                  fillOpacity = .7,
                  color = "white",
                  weight = 0.5) 
  }
  
  output$all_layers_map <- renderLeaflet(
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      dk_add_layer("Children 5-17") %>%
      dk_add_layer("Children Under 5") %>%
      dk_add_layer("Median Income for Children Under 18") %>%
      dk_add_layer("Non-English Speakers") %>%
      addLayersControl(
        overlayGroups = social_indicators_vars,
        options = layersControlOptions(collapsed = FALSE)
      )

  )
 
  
  
  output$composite_map <- renderLeaflet(
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = composite_score, 
                  fillColor = ~colorNumeric("Blues", total_score,
                                            na.color = "#eeeeee")(total_score),
                  fillOpacity = 0.7,
                  weight = 0,
                  color = "transparent",
                  label = ~str_glue("Score: {as.character(total_score)}"),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE)) 
      )
  
}


