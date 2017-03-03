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

# pre: should pass as ISO3(current) string(ALL CAPS) to country, a vector of a starting year and
#	   ending year(numbers) to year.range, and a list of filters to selected.
#	   Format for filter: ['col.name': 'attribute to filter']
#	   Example for filter: [attacktype1_txt: 'Assassination', targtype1_txt: 'Private Citizens & Property']
# post: Will return a list of plotly pie charts indicating Attacks Type, Targets,
#		and Used Weapons
Attack.Pies <- function(country, year.range, selected){}