library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(countrycode)

source('./API.R')
DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = TRUE)
Sys.setenv("plotly_username" = "s92025592025")
Sys.setenv("plotly_api_key" = plotly.key)


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

# sort country data first
#country.data <- attack.per.country %>%
#				mutate(ISO3 = countrycode(country_txt, 'country.name', 'iso3c')) %>%
#				select(country_txt, ISO3) %>%
#				mutate(`New ISO3` = iso.alpha(country_txt, n = 3))


#write.csv(country.data, './data/country_data.csv')