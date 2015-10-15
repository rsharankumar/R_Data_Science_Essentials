#### Data Science Essentials
#### Chapter 8 - Communicating Data Analysis
#### Author: Sharan Kumar R and Raja B Koushik

setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 8")
getwd()

# Google viz Plotting
install.packages("googleVis")
library(googleVis)

data <- read.csv("Data/worlddata.csv")
newdata <- data[,c("country","co2_emissions")]
newdata <- na.omit(newdata)
head(newdata, 10)


GeoPlot=gvisGeoChart(newdata, locationvar="country", 
                 colorvar="co2_emissions",
                 options=list(projection="kavrayskiy-vii", width=1000, height=700))
plot(GeoPlot)

# Few more googleViz 

Filtereddata <- newdata[newdata$co2_emissions > quantile(newdata$co2_emissions,prob=1-5/100),]
head(Filtereddata)
# bar Chart
Bar <- gvisBarChart(Filtereddata)
plot(Bar)

# Column Chart
Clmn <- gvisColumnChart(Filtereddata)
plot(Clmn)

# area chart
Area <- gvisAreaChart(Filtereddata)
plot(Area)

# pie cart
Pie <- gvisPieChart(Filtereddata)
plot(Pie)

# Shiny App
install.packages("shiny")
library(shiny)

runExample("App")

