library(biscale)
library(sf)
library(tidyverse)
library(cowplot)
library(extrafont)
library(tigris)



# Limited English by kids under 5 -------------------------------------------------

wash_co_census_tracts <- tracts(state = "OR",
                                county = "Washington",
                                cb = TRUE) %>% 
  clean_names() 

limited_english_kids_under_5 <- left_join(speak_english_less_than_well,
                                          children_under_5,
                                          by = "geoid") %>% 
  select(geoid, limited_english_speakers, under_5) %>% 
  right_join(wash_co_census_tracts)

limited_english_kids_under_5_biscale <- bi_class(limited_english_kids_under_5, 
                                                 x = limited_english_speakers, 
                                                 y = under_5, 
                                                 style = "quantile", 
                                                 dim = 3) %>% 
  select(geoid, limited_english_speakers, under_5, bi_class, geometry) %>% 
  st_as_sf()

map <- ggplot() +
  geom_sf(data = limited_english_kids_under_5_biscale, 
          mapping = aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE) +
  labs(title = "Limited English Speakers and Children Under 5 ",
       subtitle = "Washington County, Oregon") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme() 

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "More Limited English Speakers",
                    ylab = "More Children Under 5",
                    size = 10)



ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.25, 0.25) 

ggsave("biscale-sample.png",
       width = 12,
       dpi = 300)

# Sample ------------------------------------------------------------------

data(stl_race_income)


loadfonts()

data <- bi_class(stl_race_income, x = pctWhite, y = medInc, style = "quantile", dim = 3)

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Race and Income in St. Louis, MO",
    subtitle = "Dark Blue (DkBlue) Palette"
  ) +
  bi_theme() +
  theme(axis.title.x = element_text(family = "Formata"),
        plot.title = element_text(family = "Formata"))

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher % White ",
                    ylab = "Higher Income ",
                    size = 8) +
  theme(axis.title.x = element_text(family = "Formata"),
        plot.title = element_text(family = "Formata"))



ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2) +
  theme(axis.title.x = element_text(family = "Formata"),
        plot.title = element_text(family = "Formata"))
