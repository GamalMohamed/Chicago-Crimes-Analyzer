rm(list=ls())

library(xts)
library(dplyr)
library(highcharter)
library(ggplot2)
library(tidyr)
library(viridis)
library(lubridate)
library(gridExtra)

##Reading dataset
crimes2005_2007 = read.csv("Dataset/2005_to_2007.csv", quote="",stringsAsFactors=F, row.names=NULL)
colnames(crimes2005_2007) <- c(colnames(crimes2005_2007)[-1],NULL)
crimes2008_2011 = read.csv("Dataset/2008_to_2011.csv",stringsAsFactors=F)
crimes2012_2017 = read.csv("Dataset/2012_to_2017.csv",stringsAsFactors=F)
crimes2012_2016 = crimes2012_2017[crimes2012_2017$Year!='2017',] #Removing 2017 records as it isn't complete & reliable!

ccc=na.omit(crimes2005_2007[,-24])
crimes = rbind(crimes2005_2007[,-24],crimes2008_2011,crimes2012_2016)

## adding date attributes
crimes$Day = factor(day(as.POSIXlt(crimes$Date, 
                                            format="%m/%d/%Y %I:%M:%S %p")))
crimes$Weekday = factor(wday(as.POSIXlt(crimes$Date, 
                                        format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
crimes$Month = factor(month(as.POSIXlt(crimes$Date, 
                                                format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
crimes$Year = factor(year(as.POSIXlt(crimes$Date, 
                                              format="%m/%d/%Y %I:%M:%S %p")))
crimes$Date = as.Date(crimes$Date, "%m/%d/%Y %I:%M:%S %p")


## Creating Crimes time series
crimes_by_Date = na.omit(crimes) %>% group_by(Date) %>% summarise(Total = n())
crimes_tseries = xts(crimes_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

## Creating Arrests time series
arrests_data = na.omit(crimes[crimes$Arrest == 'True',])
arrests_by_Date = arrests_data %>% group_by(Date) %>% summarise(Total = n())
arrests_tseries = xts(arrests_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))


## Some useful insights
crimes_by_location = crimes %>% group_by(Location.Description) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_primaryType = crimes %>% group_by(Primary.Type) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_district = crimes %>% group_by(District) %>%
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_ward = crimes %>% group_by(Ward) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_fbi = crimes %>% group_by(FBI.Code) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_arrest = crimes %>% group_by(Arrest) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_domestic = crimes %>% group_by(Domestic) %>% 
                      summarise(Total = n()) %>% arrange(desc(Total))

crimes_by_year = na.omit(crimes) %>% group_by(Year) %>% 
                      summarise(Total = n())


## Plot 1: Crimes-Arrests times series Plot
hchart(crimes_tseries, name = "Crimes") %>% 
  hc_add_series(arrests_tseries, name = "Arrests") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Chicago Crimes and Arrests Times Series plot") %>%
  hc_legend(enabled = TRUE)

## Plot 2: Arrests time series focused
hchart(arrests_tseries) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Times Series plot of Arrests made in Chicago")

## Plot 3: Crimes by Year
hchart(crimes_by_year, "column", hcaes(x = Year, y = Total, color = Year)) %>%
  hc_colorAxis(stops = color_stops(n = 20, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Number of Crimes by Year") %>%
  hc_legend(enabled = FALSE)

## Plot 4: Arrests vs Crimes Heatmaps (by Year & Month)
crimes_count = na.omit(crimes) %>% group_by(Year, Month) %>% summarise(Total = n())
arrests_count = arrests_data %>% group_by(Year, Month) %>% summarise(Total = n())

arrests_plot = ggplot(arrests_count, aes(Year, Month, fill = Total)) +
        geom_tile(size = 1, color = "white") +
        scale_fill_viridis()  +
        geom_text(aes(label=Total), color='white') +
        ggtitle("Arrests by Year and Month")

crimes_plot = ggplot(crimes_count, aes(Year, Month, fill = Total)) +
        geom_tile(size = 1, color = "white") +
        scale_fill_viridis()  +
        geom_text(aes(label=Total), color='white') +
        ggtitle("Crimes by Year and Month")

grid.arrange(crimes_plot, arrests_plot, ncol = 2)


## Plot 5: Locations with most crimes (Top 20)
hchart(crimes_by_location[1:20,], "column", 
       hcaes(x = Location.Description, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Locations with most Crimes - Top 20") %>%
  hc_legend(enabled = FALSE)


## Create Time series for the 1st top 4 locations
streets = crimes[crimes$Location.Description=="STREET",]
streets_by_Date = na.omit(streets) %>% group_by(Date) %>% summarise(Total = n())
streets_tseries = xts(streets_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

residence = crimes[crimes$Location.Description=="RESIDENCE",]
residence_by_Date = na.omit(residence) %>% group_by(Date) %>% summarise(Total = n())
residence_tseries = xts(residence_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

apartment = crimes[crimes$Location.Description=="APARTMENT",]
apartment_by_Date = na.omit(apartment) %>% group_by(Date) %>% summarise(Total = n())
apartment_tseries = xts(apartment_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

sidewalk = crimes[crimes$Location.Description=="SIDEWALK",] 
sidewalk_by_Date = na.omit(sidewalk) %>% group_by(Date) %>% summarise(Total = n())
sidewalk_tseries = xts(sidewalk_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

## Plot 6:Top 4 locations time series
hchart(streets_tseries, name = "Streets") %>% 
  hc_add_series(residence_tseries, name = "Residence") %>% 
  hc_add_series(apartment_tseries, name = "Apartment") %>%
  hc_add_series(sidewalk_tseries, name = "Sidewalk") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crimes in Streets/Residence/Apartment/Sidewalk") %>%
  hc_legend(enabled = TRUE)

## Plot 7: Crime Types count
hchart(crimes_by_primaryType, "column", hcaes(Primary.Type, y = Total, color = Total)) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crime Types") %>%
  hc_legend(enabled = FALSE)


## Create Time series for the 1st top 4 crime types
thefts = crimes[crimes$Primary.Type=="THEFT",] 
thefts_by_Date = na.omit(thefts) %>% group_by(Date) %>% summarise(Total = n())
thefts_tseries = xts(thefts_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

battery = crimes[crimes$Primary.Type=="BATTERY",] 
battery_by_Date = na.omit(battery) %>% group_by(Date) %>% summarise(Total = n())
battery_tseries = xts(battery_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

criminals = crimes[crimes$Primary.Type=="CRIMINAL DAMAGE",]
criminals_by_Date = na.omit(criminals) %>% group_by(Date) %>% summarise(Total = n())
criminals_tseries = xts(criminals_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

narcotics = crimes[crimes$Primary.Type=="NARCOTICS",] 
narcotics_by_Date = na.omit(narcotics) %>% group_by(Date) %>% summarise(Total = n())
narcotics_tseries = xts(narcotics_by_Date$Total, order.by=as.POSIXct(crimes_by_Date$Date))

## Plot 8: Top 4 crime types time series
hchart(thefts_tseries, name = "Thefts") %>% 
  hc_add_series(battery_tseries, name = "Battery") %>% 
  hc_add_series(criminals_tseries, name = "Criminal Damage") %>%
  hc_add_series(narcotics_tseries, name = "Narcotics") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Crimes in Thefts/Battery/Criminal Damage/Narcotics") %>%
  hc_legend(enabled = TRUE)


## Plot 9: Homocides count by Year
homicide = crimes[crimes$Primary.Type=="HOMICIDE",] 
homicide_year = homicide %>% group_by(Year) %>% summarise(Total = n())
hchart(homicide_year, "column", hcaes(Year, Total, color = Year)) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Homicides")


# Plot 10: Homocides Heatmap by Year & month
homicide_count <- homicide %>% group_by(Year, Month) %>% summarise(Total = n())
ggplot(homicide_count, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Homicides")

