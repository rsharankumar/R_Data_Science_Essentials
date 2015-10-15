
#### Server Side 



shinyServer(function(input, output, session) {
  # First Tab
  output$plot <- renderPlot({
    setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 8")
    data <- read.csv("Data/worlddata.csv")
    data <- data[,c("gdp_in_millions","co2_emissions")]
    data <- na.omit(data)
    data <- data[data$gdp_in_millions > quantile(data$gdp_in_millions,prob=1-10/100),]
    plot(data, type="p")
  })
  # Second Tab
  output$summary <- renderPrint({
    setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 8")
    data <- read.csv("Data/worlddata.csv")
    data <- data[,c("gdp_in_millions","co2_emissions")]
    data <- na.omit(data)
    data <- data[data$gdp_in_millions > quantile(data$gdp_in_millions,prob=1-10/100),]
    summary(data)
  })
  # Third Tab
  output$table <- renderDataTable({
    setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 8")
    data <- read.csv("Data/worlddata.csv")
    data <- data[,c("country","gdp_in_millions","co2_emissions")]
    data <- na.omit(data)
    data <- data[data$gdp_in_millions > quantile(data$gdp_in_millions,prob=1-10/100),]
    data
  }, options=list(pageLength=10))
})

