setwd("~/GitHub/Rshiny")

install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("googleVis")
install.packages("reshape2")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(googleVis)
library(reshape2)

##Read the csv file  co2_emission with stringAsFactors to FALSE because we dont want to
##string to be treated as factors
co2 = read.csv("~/Documents/r/projects/Project 1/Team SeeRs project 1/SeeRs Code/co2_emission.csv", stringsAsFactors = F)

#Used gsub() to remove the comma present in the numeric columns of the csv file
co2$CO2.Emissions = as.numeric( gsub( ",", "", co2$CO2.Emissions) )
co2$Emissions.from.bunker.fuels = as.numeric( gsub( ",", "", co2$Emissions.from.bunker.fuels ))
co2$Emissions.from.gas.flaring = as.numeric( gsub( ",", "", co2$Emissions.from.gas.flaring))
co2$Emissions.from.cement.production = as.numeric( gsub( ",", "", co2$Emissions.from.cement.production))
co2$Emissions.from.gas.fuel.consumption = as.numeric( gsub( ",", "", co2$Emissions.from.gas.fuel.consumption))
co2$Emissions.from.solid.fuel.consumption = as.numeric(gsub( ",", "", co2$Emissions.from.solid.fuel.consumption))
co2$Emissions.from.liquid.fuel.consumption  = as.numeric((gsub( ",", "", co2$Emissions.from.liquid.fuel.consumption)))
co2$Gdp.Ppp = as.numeric(gsub( ",", "", co2$Gdp.Ppp))

shinyApp(ui = ui, server = server)

