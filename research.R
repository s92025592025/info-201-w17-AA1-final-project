library(dplyr)
library(plotly)
library(ggplot2)

DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = TRUE)

# This block will try to find out the terrorist attact per year

attack.per.year <- DATA %>%
				   group_by(iyear) %>%
				   summarise(times = n()) %>%
				   mutate(`Collected Institution` = ifelse(iyear <= 1997, 'PGIS',
				   										ifelse(iyear <= 2008, 'CETIS',
				   											ifelse(iyear <= 2011, "ISVG", 'START'))))

write.csv(attack.per.year, './data/attack.per.year.csv')