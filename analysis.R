# Final Project
# Info 201 AA1
# 2017/03/02
# This is a file contains functions to generate plots to show on a pages

library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)

DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CONVERT <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)
DATA.w.ISO3 <- left_join(DATA, ISO3.CONVERT)


world <- map_data("world")
world <- mutate(world, ISO3 = iso.alpha(region, 3))

attack.country.year <- DATA.w.ISO3 %>%
                       group_by(country_txt, ISO3, iyear) %>%
                       summarize(Attacks = n()) %>% 
                       select(Country = country_txt, ISO3, Year = iyear, Attacks)
  


Global.Terrorism.Attacks <- function(year) {

attacks <- filter(attack.country.year, Year == '2015') %>% select(Attacks) %>% arrange(Attacks)

p <- attack.country.year %>%
  filter(Year == '2015') %>%  #Replace with ui.r variable
  right_join(world, by = 'ISO3') %>% 
  ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, text = sprintf("Country: %s<br>Attacks: %s", Country, Attacks), fill = ifelse(is.na(Attacks), 0, Attacks))) +
    scale_fill_gradientn(colors = c('green3', 'yellow', 'red'), values = rescale(attacks$Attacks), name = "Attacks") +
    ggtitle(paste("Global Terrorism Attacks", year)) +
    theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
    coord_quickmap()

ggplotly(p, tooltip = "text")

}