# ---------------------------------------------------------------------------------------------
# Title: CS710 Final Project
# Author: Hanna Lu
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------
# About this project:

# This project attempts to answer the following three questions:
#   1. How did the Covid situation evolve in the world since January 2020?
#   2. What variable from the dataset is closely related to the Covid deaths?
#   3. How is that variable related to Covid death?
# ---------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------------------------
list.of.packages <- c("ggplot2", "colorspace", "dplyr", "tidyverse", "reshape2", 
                        "png", "lubridate","ggcorrplot","ggrepel", "plotly")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)  


# Load the libraries
library(ggplot2)
library(colorspace)
library(dplyr)
library(tidyverse)
library(reshape2)
library(png)
library(lubridate)
library(ggcorrplot)
library(ggrepel)
library(plotly)


# ---------------------------------------------------------------------------------------------
# Load the data and preprocess the data
# ---------------------------------------------------------------------------------------------

covid <- read.csv("data/owid-covid-data.csv")

# Fill the missing values in the new_cases_per_million variable with 0
covid$new_cases_per_million[is.na(covid$new_cases_per_million)] <- 0

# Create a variable for average number of new cases per day in each month
covid_2 <- covid %>%
  select(c("location", "date", "new_cases_per_million")) %>%
  mutate(year_month = paste(year(date), month(date), sep="-")) %>%
  group_by(location, year_month) %>%
  summarise(daily_new_cases = mean(new_cases_per_million, na.rm = T)) %>%
  dcast(location ~ year_month, value.var = "daily_new_cases")

# Fill the missing values with 0
covid_2[is.na(covid_2)] <- 0

# Reorder the variables by the order of time
covid_2 <- covid_2[,c(1,2,6,7,8,9,10,11,12,13,3,4,5,14,15,16,17,18)]


# ---------------------------------------------------------------------------------------------
# Part I: The Covid situation in the world since January 2020
# ---------------------------------------------------------------------------------------------

# Load the world map data
world <- map_data("world")

# Identify the countries that are in the Covid data but not in the world map data
levels(factor(covid_2[!covid_2$location %in% world$region,]$location))

# Change the names of some countries according to the world map data
covid_2$location <- case_when(
  covid_2$location == "Bahamas, The"            ~ "Bahamas",
  covid_2$location == "United Kingdom"          ~ "UK",
  covid_2$location == "United States"           ~ "USA",
  covid_2$location == "Trinidad & Tobago"       ~ "Trinidad",
  covid_2$location == "Congo"                   ~ "Republic of Congo",
  covid_2$location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
  covid_2$location == "Russian Federation"      ~ "Russia",
  covid_2$location == "Yemen, Rep."             ~ "Yemen",
  covid_2$location == "Venezuela, RB"           ~ "Venezuela",
  covid_2$location == "Lao PDR"                 ~ "Laos",
  covid_2$location == "Iran, Islamic Rep."      ~ "Iran",
  covid_2$location == "Egypt, Arab Rep."        ~ "Egypt",
  covid_2$location == "Czechia"                 ~ "Czech Republic",
  covid_2$location == "Faeroe Islands"          ~ "Faroe Islands",
  covid_2$location == "North Macedonia"         ~ "Macedonia",
  TRUE                                          ~ covid_2$location
)

# Merge world map data with Covid data
data <- merge(world, covid_2, by.x="region", by.y="location", all.x = TRUE)
# data[is.na(data$daily_new_cases)] <- 0


# Plot the Covid data on the world map
map_plot <- function(data, world, m){
  if(m <= 12){
    year = "2020"
    month = month.abb[m]
  } else{
    year = "2021"
    month = month.abb[m-12]
  }

  # Select the data month by month
  data <- data[, c(1,2,3,4,5,6,m+6)]
  data$cases <- data[,7]

  
  # Plot the data month by month
  ggplot(data, aes(x=long, y=lat)) +
    geom_map(map = world,
             aes(map_id = region, fill = cases))  +
    scale_fill_continuous_diverging(palette = "Blue-Red",
                                    mid = mean(data[,7], na.rm = TRUE),
                                    n_interp = 51,
                                    cmax = 100, p1=0.5, p2=0.5,
                                    rev = F) +
    labs(title= paste0("Covid New Cases per Million per Day in ", month, ", ", year), 
         x="", y="", fill = "Average Number of\nNew Cases\nper Million\nper Day",
         subtitle = "(Countries in dark gray do not have Covid data)") +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA)) 
}

# ---------------------------------------------------------------------------------------------
# Define the width and height of the images
w <- 10
h <- 6

# Plot the Covid map and Save the plots in .jpeg format
# These .jpeg files can then be loaded in ImageJ to make a .avi file
for(i in 1:17){
  jpeg(filename=paste0("results/plots/Covid_",i,".jpeg"),
      width=w,height=h,unit="in",res=200)
  print(map_plot(data=data, map=world, m=i))
  dev.off()
}


# ---------------------------------------------------------------------------------------------
# Part II  What is Related to Total Deaths per Million?
# ---------------------------------------------------------------------------------------------

# Choose the data of the most current day in the dataset
covid_3 <- covid %>%
  select(c("continent", "location", "date", 
           "total_deaths", "total_cases",
           "total_deaths_per_million",
           "population_density", "gdp_per_capita", 
           "diabetes_prevalence",
           "hospital_beds_per_thousand", "human_development_index")) %>%
  filter(date == max(date)) %>%
  filter(!continent == "") %>%
  na.omit()

# Create a new variable for Death Rate
covid_3$death_rate <- round(covid_3$total_deaths/covid_3$total_cases,4)

# Rename the variables
names(covid_3) <- c("Continent","Country","date", "total_deaths", "total_cases",
                    "Total Deaths/Million",
                    "Population Density","GDP per Capita",
                    "Diabetes Prevalence", "Hospital Beds/1000", 
                    "HDI", "Death Rate")

# Create Correlation heat map (this will show up in the Plots window)
ggcorrplot(cor(covid_3[,6:11],covid_3[,6:11]), 
           sig.level=0.05, lab_size = 4.5, p.mat = NULL, 
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 14, 
           legend.title = "Correlation") +
  labs(title = "Correlaton",
       subtitle = "- HDI has the strongest correlation with Total Deaths/Million -\n(HDI (Human Development Index) is a combination of Life Expectancy, Education, and Standard of Living)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5),
        # Order: top, right, bottom, left
        axis.text.x = element_text(size=11, family="TT Times New Roman", 
                                   margin=margin(-2,0,0,0)),  
        axis.text.y = element_text(size=11, family="TT Times New Roman", 
                                   margin=margin(0,-2,0,0))) +
  geom_vline(xintercept=1:ncol(covid_3)-0.5, colour="white", size=2) +
  geom_hline(yintercept=1:ncol(covid_3)-0.5, colour="white", size=2) 


# ---------------------------------------------------------------------------------------------
# Part III  How is HDI related to Total Deaths per Million?
# ---------------------------------------------------------------------------------------------

# Round the values of Log Total Deaths/Million to the 3rd decimal place
covid_3$`Log Total Deaths/Million` <- round(log(covid_3$`Total Deaths/Million`),3)

# Plot the scatter plot of Log Total Deaths/Million vs HDI
p <- ggplot(covid_3, aes(text=paste("Death Rate:", `Death Rate`,
                                    "\nCountry:", Country)))+ 
  geom_point(aes(x=HDI, y=`Log Total Deaths/Million`,
                 color=Continent), alpha=0.5, size=3) +
  # ylim(c(-1.2,8.2)) +
  # xlim(c(0.35,0.95)) +
  labs(x="Human Development Index (HDI)", 
       y="Log (Total Covid Deaths per Million)",
       color="", 
       title=paste("Total Covid Deaths per Million (as of 5/7/2021) vs Human Development Index (HDI)",
        "\n- The more developed countries have more deaths -",
        "\n(Hover over the dots for details)")) +
  scale_color_manual(values = c("darkred", "darkorange", "blue", "blueviolet",
                                "deeppink", "darkgreen")) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(size=12, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))


plot <- ggplotly(p) %>% 
  layout(legend = list(orientation = 'v',title=list(text='<b> Continent\n </b>')),
         xaxis = list(range = c(0.37,0.98)),
         yaxis = list(range = c(-1.5,8.7)))
plot


# Save the interactive plot to the results folder
htmlwidgets::saveWidget(plot, "results/scatter_plot.html")

# ---------------------------------------------------------------------------------------------
