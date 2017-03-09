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


# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#	   and ending year(numbers) to year.range, and a list of filters to selected.
#	   Format for filter: ['col.name'='attribute to filter']
#	   Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: Will return a list of plotly pie charts indicating Attacks Type, Targets,
#		and Used Weapons
Attack.Info.Pies <- function(country.iso3, year.range, selected){
	filtered <- Pie.Data.Filter(country.iso3, year.range, selected)

	return(list(type = Attack.Type.Pie(filtered), targets = Attack.Target.Pie(filtered),
				weap = Attack.Weap.Pie(filtered)))
}

# pre: should give data a filtered data
# post: will return a plotly contains a pie chart showing the ratio of each kind of
#		attack
Attack.Type.Pie <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   attacktype1_txt) %>% #, attacktype2_txt, attacktype3_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(plot_ly(gathered, labels = ~type, values = ~time, type = 'pie',
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
		   layout(title = "Attack Types",
		 		  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         		  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}

# pre: should give data a filtered data
# post: will return a plotly pie chart showing the ratio of each kind of attack targets
Attack.Target.Pie <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   targtype1_txt) %>% #, targtype2_txt, targtype3_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(plot_ly(gathered, labels = ~type, values = ~time, type = 'pie',
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
		   layout(title = "Attack Targets",
		 		  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         		  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}


# pre: should give data a filtered data
# post: will return a plotly pie chart showing the ratio od each kind of Weapons used
#		in attacks
Attack.Weap.Pie <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   weaptype1_txt) %>% #, weaptype2_txt, weaptype3_txt, weaptype4_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(plot_ly(gathered, labels = ~type, values = ~time, type = 'pie',
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
		   layout(title = "Attack Weapons",
		 		  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         		  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}

# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#	   and ending year(numbers) to year.range, and a list of filters to selected.
#	   Format for filter: ['col.name'='attribute to filter']
#	   Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: will return a list of information that are not duplicated in Attack type, targets and weapon
Attack.Info.List <- function(country.iso3, year.range, selected){
	filtered <- Pie.Data.Filter(country.iso3, year.range, selected)

	return(list('type' = Attack.Type.List(filtered), 'target'=Attack.Target.List(filtered),
				'weap' = Attack.Weap.List(filtered)))
}

# pre: should give data a filtered data
# post: will return a list of non-duplicate attack type under the data
Attack.Type.List <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   attacktype1_txt) %>% #, attacktype2_txt, attacktype3_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(c('ALL', gathered[['type']]))
}


# pre: should give data a filtered data
# post: will return a list of non-duplicate attack target under the data
Attack.Target.List <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   targtype1_txt) %>% #, targtype2_txt, targtype3_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(c('ALL', gathered[['type']]))
}

# pre: should give data a filtered data
# post: will return a list of non-duplicate attack weapon under the data
Attack.Weap.List <- function(data){
	gathered <- data %>%
				gather(key = num, value = type,
					   weaptype1_txt) %>% #, weaptype2_txt, weaptype3_txt, weaptype4_txt) %>%
				group_by(type) %>%
				summarise(time = n()) %>%
				filter(type != '.')

	return(c('ALL', gathered[['type']]))
}

# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#	   and ending year(numbers) to year.range, and a list of filters to selected.
#	   Format for filter: ['col.name'='attribute to filter']
#	   Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: will return a data frame that was filtered by give filter
Pie.Data.Filter <- function(country.iso3, year.range, selected){
	# Filters out the data within the year.range
	filtered <- DATA.w.ISO3 %>%
				filter(iyear >= year.range[1], iyear <= year.range[2])

	# filter out the selected country if needed
	if(country.iso3 != 'WORLD'){
		filtered <- filtered %>%
					filter(New.ISO3 == country.iso3)
	}

	# filter out the selected data
	for(key in names(selected)){
		filtered <- filtered %>%
					filter_(paste0(key, '1_txt=="' ,selected[key], '"'))
	}
	#write.csv(filtered, 'testing.csv')
	return(filtered)
}

# pre: when need to obtain a list of countries that are on in the attack database
# post: will return a list of countries in the format of "Name" = ISO3
All.Country.List <- function(){
	country <- ISO3.CONVERT %>%
			   filter(ISO3 == New.ISO3)

	country.name <- country[['country_txt']]
	country.iso3 <- country[['New.ISO3']]

	country.list <- list()


	for(i in 1:length(country.name)){
		country.list[[country.name[i]]] <- country.iso3[i]
	}

	return(country.list)
}

Global.Terrorism.Attacks <- function(year) {
	world <- map_data("world")
	world <- mutate(world, ISO3 = iso.alpha(region, 3))

	attack.country.year <- DATA.w.ISO3 %>%
	                       group_by(country_txt, ISO3, iyear) %>%
	                       summarize(Attacks = n()) %>% 
	                       select(Country = country_txt, ISO3, Year = iyear, Attacks)

	attacks <- filter(attack.country.year, Year == year) %>% select(Attacks) %>% arrange(Attacks)

	p <- attack.country.year %>%
	  	 filter(Year == year) %>%  #Replace with ui.r variable
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

	return(ggplotly(p, tooltip = "text"))

}
