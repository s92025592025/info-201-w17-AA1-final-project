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
library(reshape2)

DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CONVERT <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)
DATA.w.ISO3 <- left_join(DATA, ISO3.CONVERT)
countries <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)

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

	return(plot_ly(gathered, labels = ~type, values = ~time,
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
				     add_pie(hole = 0.6) %>% 
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

	return(plot_ly(gathered, labels = ~type, values = ~time,
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
				     add_pie(hole = 0.6) %>% 
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

	return(plot_ly(gathered, labels = ~type, values = ~time,
				   textposition = 'inside', textinfo = 'label+percent',
			       showlegend = FALSE) %>%
				     add_pie(hole = 0.6) %>% 
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

Attack.Info.List("USA", c(2015, 2015), list('targtype'='Business'))
Attack.Info.Pies("USA", c(2015, 2015), list('targtype'='Business'))

# pre:  Insert date range in terms of years (min & max year)
# post: The function will return a ggplotly world map illustrating the number of terrorist attacks
#       during the selected years
Global.Terrorism.Attacks <- function(year.min, year.max) {
  
  # Importing geographic data
  world <- map_data("world")
  world <- mutate(world, ISO3 = iso.alpha(region, 3))
  
  # Grouping & filtering terrorism data using the required date range
  attacks <- DATA.w.ISO3 %>%
    filter(iyear >= year.min & iyear <= year.max) %>% 
    group_by(country_txt, ISO3) %>%
    summarize(Attacks = n()) %>% 
    select(Country = country_txt, ISO3, Attacks) %>% 
    arrange(Attacks)
  
  
  # Plotting a world map by combining geographic and terrorism data
  p <- attacks %>%
    right_join(world, by = 'ISO3') %>%
    left_join(countries, by = 'ISO3') %>%
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group,
                     text = sprintf("Country: %s<br>Attacks: %s", ifelse(is.na(Country), region, Country), ifelse(is.na(Attacks), "No Data", Attacks)),
                     fill = ifelse(is.na(Attacks) & is.na(country_txt) == FALSE, 0, Attacks))) +
    scale_fill_gradientn(name = "Attacks", colors = c('green3', 'yellow', 'red'), values = rescale(attacks$Attacks)) +
    ggtitle(paste("Global Terrorism Attacks", ifelse(year.min == year.max, year.min, paste(year.min, "to", year.max)))) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    coord_quickmap()
  
  
  return(ggplotly(p, tooltip = "text"))
}

compare.rates <- function(data.type,attack.type){
  
  if(data.type == "multiple") {
    
    multiple.data <- select(DATA, multiple, attacktype1_txt) %>% 
      group_by(attacktype1_txt, multiple) %>% 
     # filter_(paste0("attacktype1_txt ==", "'", attack.type, "'")) %>% 
      summarise(count = n())
    
    fail <- filter(multiple.data , multiple == 0) %>% summarise(No = count / sum(multiple.data$count) * 100)
    success <- filter(multiple.data, multiple == 1)%>% summarise(Yes = count / sum(multiple.data$count) * 100)
  }
  
  if(data.type == "success") {
    success.data <- select(DATA, success, attacktype1_txt) %>% 
      group_by(attacktype1_txt, success) %>% 
      summarise(count = n())
    
    fail <- filter(success.data , success == 0) %>% summarise(No = count / sum(success.data$count) * 100)
    success <- filter(success.data, success == 1) %>% summarise(Yes = count / sum(success.data$count) * 100)
  }
  
  if(data.type == "suicide") {
      suicide.data <- select(DATA, suicide, attacktype1_txt) %>% 
      group_by(attacktype1_txt, suicide) %>% 
      summarise(count = n())
  
    fail <- filter(suicide.data , suicide == 0) %>% summarise(No = count / sum(suicide.data$count) * 100)
    success <- filter(suicide.data, suicide == 1)%>% summarise(Yes = count / sum(suicide.data$count) * 100)
  }
  
  get.percentage.data <- left_join(fail, success, by = "attacktype1_txt") 
        if(attack.type != "ALL"){
          get.percentage.data <- get.percentage.data %>% 
                                filter_(paste0("attacktype1_txt ==", "'", attack.type, "'"))
        }
  get.percentage.data <- melt(get.percentage.data, id.vars = "attacktype1_txt")
  colnames(get.percentage.data) <- c("Type", "YesNo", "Percentage")
  p <- plot_ly(get.percentage.data, labels = ~YesNo, values = ~Percentage,
               textposition = 'inside', textinfo = 'label+percent',
               showlegend = FALSE) %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Attack Weapons",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  return(p)
}
