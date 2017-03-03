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

DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CONVERT <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)
DATA.w.ISO3 <- left_join(DATA, ISO3.CONVERT)

# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country, a vector of a starting year
#	   and ending year(numbers) to year.range, and a list of filters to selected.
#	   Format for filter: ['col.name'='attribute to filter']
#	   Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: Will return a list of plotly pie charts indicating Attacks Type, Targets,
#		and Used Weapons
Attack.Info.Pies <- function(country, year.range, selected){
	# Filters out the data within the year.range
	filtered <- DATA %>%
				filter(iyear >= year.range[1], iyear <= year.range[2])

	# filter out the selected country if needed
	if(country != 'WORLD'){
		filtered <- filtered %>%
					filter(country_txt == country)
	}

	print(2)
	print(filtered)

	# filter out the selected data
	for(key in names(selected)){
		filtered <- filtered %>%
					filter_(key == selected[key])
	}

	write.csv(filtered, './data/testing.csv')
}

Attack.Info.Pies("United States", c(2015, 2015), list())