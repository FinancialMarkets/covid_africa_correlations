library(viridisLite)
library(highcharter)
library(tidyverse)
library(htmlwidgets)

options(browser = "/usr/bin/firefox")

data <- read_csv("../african_latest_data.csv")
data$dateRep <- as.Date(data$dateRep, format="%d/%m/%Y")
data$popData2018 <- data$popData2018 / 1000000
data$cases_per_million <- data$cumulative_cases / data$popData2018
data$deaths_per_million <- data$cumulative_deaths / data$popData2018

data_graphic <- data[, c("countryterritoryCode", "geoId", "dateRep", "countriesAndTerritories", "cases_per_million", "cumulative_cases", "cases", "deaths", "cumulative_deaths", "deaths_per_million")]

## remove france

data_graphic <- subset(data_graphic, countriesAndTerritories != "France")
data_graphic <- subset(data_graphic, countriesAndTerritories != "Seychelles")
# data_graphic <- subset(data_graphic, countriesAndTerritories != "Mauritius")

## rename
data_graphic$`iso-a3` <- data_graphic$countryterritoryCode

## get latest day
latest_day <- max(unique(data_graphic$dateRep))

data_graphic_latest <- subset(data_graphic, dateRep == latest_day) 
## merge in WB data

wb_data <- read_csv("../WB_data/wb_data.csv")
wb_data$geoId <- wb_data$`hc-key`

data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "geoId", all.x = TRUE)

wb_data$countriesAndTerritories <- wb_data$country_

## now merge Namibia
data_graphic_latest <- merge(data_graphic_latest, wb_data, by = "countriesAndTerritories", all.x = TRUE)


data_graphic_latest$per_pop_65 <-  data_graphic_latest$SP.POP.65UP.TO.ZS.x
data_graphic_latest$pop_urban <- data_graphic_latest$SP.URB.TOTL.IN.ZS.x
data_graphic_latest$health_exp_per_cap <- data_graphic_latest$SH.XPD.CHEX.PC.CD.x
data_graphic_latest$int_ext_debt <- data_graphic_latest$DT.INT.DECT.EX.ZS.x

## graphic

cor_data <- data_graphic_latest[, c("per_pop_65", "pop_urban", "health_exp_per_cap", "cases_per_million", "deaths_per_million")]
cor_data <- cor_data[complete.cases(cor_data), ]
names(cor_data) <- c("Percent\ Population\ over\ 65", "Percent\ Population\ Urban", "Health\ Expenditures\ Per\ Capita", "COVID-19\ Cases\ Per\ Million", "COVID-19\ Deaths\ Per\ Million")

correlations <- cor(cor_data)

#correlations$colors <- colorize(correlations, colors = c("#999999", "#0066CC"))

corChart <- hchart(correlations) %>%#, color = colors) %>%
  hc_add_theme(hc_theme_google()) %>%
#  hc_title(text = "Correlation Coefficient Matrix") %>%
  hc_plotOptions(series=list(states = list(hover = list(enabled = FALSE))))
corChart

## Save vis
saveWidget(corChart, file="correlations.html")



## map to create: per capita health expenditure per case (in hundred millions): the higher it is the better the country is able to care for the cases---assuming more spending means more readiness
