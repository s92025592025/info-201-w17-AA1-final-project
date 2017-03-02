library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(countrycode)

source('./API.R')
DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CODE <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)


# This block will try to find out the terrorist attact per year

attack.per.year <- DATA %>%
				   group_by(iyear) %>%
				   summarise(times = n()) %>%
				   mutate(`Collected Institution` = ifelse(iyear <= 1997, 'PGIS',
				   										ifelse(iyear <= 2008, 'CETIS',
				   											ifelse(iyear <= 2011, "ISVG", 'START'))))

write.csv(attack.per.year, './data/attack.per.year.csv')

# This block will show graph of attacks each year
ggplot(data = attack.per.year) +
 	   geom_point(mapping = aes(x = iyear, y = times, 
		 						color = `Collected Institution`)) +
	   labs(title = "Attack time in time",
			x = 'Year', y = "Times")

# this block will contain table of attacks each country from 1970 to 2015
attack.per.country <- DATA %>%
					  group_by(country_txt) %>%
					  summarise(`Attacked Times` = n())
write.csv(attack.per.country, './data/attack_per_country.csv')

# this block will show map distribution about world titol terrorist distribution
world.map <- map_data('world2') %>%
			 mutate(ISO3 = iso.alpha(region, n = 3))

attack.per.country <- left_join(attack.per.country, ISO3.CODE)

ggplot(data = left_join(world.map, attack.per.country, by = c('ISO3' = 'New.ISO3'))) +
	geom_polygon(mapping = aes(x = long, y = lat, group = group,
							   fill = `Attacked Times`)) + 
	labs(title = 'Total Attacked Times From 1970 to 2015') +
	theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())