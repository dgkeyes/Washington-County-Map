library(tigris)
library(tidyverse)
library(janitor)
library(sf)

washington_county <- tracts(state = "OR",
                            county = "Washington")

oregon_zctas <- zctas(state = "OR",
                       cb = TRUE)


washington_county_zctas <- st_intersection(oregon_zctas,
                                         washington_county) 


ggplot() +
  geom_sf(data = washington_county_zctas,
          # alpha = 0,
          fill = "yellow",
          color = "white") +
  geom_sf(data = washington_county,
          color = "blue",
          alpha = 0) +
  theme_void()
