library(dplyr)
library(plotly)
library(ggplot2)

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
pdf(paste('./data/', 'attack_per_year.pdf', sep = ''))
ggplot(data = attack.per.year) +
 	   geom_point(mapping = aes(x = iyear, y = times, 
		 						color = `Collected Institution`)) +
	   labs(title = "Attack time in time",
			x = 'Year', y = "Times")