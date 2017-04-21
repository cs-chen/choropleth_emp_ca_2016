#Assignment 3
#Probelm 2

library(shiny)

#The data set
library(tidyverse)
library(leaflet)
library(maps)
library(geojsonio)
library(spdplyr)
library(sp)



#Read files
us_counties <- geojson_read("gz_2010_us_050_00_20m.json", what = "sp", stringsAsFactors = F)

ca_emp <- read.table("california_counties_monthly_employment_2016.tsv", sep = "\t", header = T, stringsAsFactors = F)
ca_emp$period <-as.POSIXct(strptime(ca_emp$period, "%Y-%m-%d"))



# Define server logic required 
shinyServer(function(input, output) {
   
  
     
  output$plot <- renderLeaflet({
     dec_ca_emp <- ca_emp %>%
       filter(months(period) == input$select) 
     
      names(dec_ca_emp)[c(2, 3)] <- c("STATE", "COUNTY")
     
      #combine the sp data with the unemployment data
      us_counties$STATE <- as.numeric(us_counties$STATE)
      us_counties$COUNTY <- as.numeric(us_counties$COUNTY)
     
      ca_emp_map <- merge(us_counties, dec_ca_emp, by = c("STATE", "COUNTY"), all.x = F)
   
      #Prepare the map
     
     name <- switch(input$item,
                    "Labor Force" = "Labor Force",
                    "Employed" = "Employed",
                    "Unemployed" = "Unemployed",
                    "Unemployed Rate" = "Unemployed Rate")
     
     data <- switch(input$item,
                 "Labor Force" = ca_emp_map$labor_force,
                 "Employed" = ca_emp_map$employed,
                 "Unemployed" = ca_emp_map$unemployed,
                 "Unemployed Rate" = ca_emp_map$unemployed_rate)
     
     bins <- switch(input$item,
                    "Labor Force" = seq(0, 6*10^6, 10^6),
                    "Employed" = seq(0, 5*10^6, 10^6),
                    "Unemployed" = seq(0, 300000, 50000),
                    "Unemployed Rate" = seq(0, 30, 5))
     
     pal <- colorBin("YlOrRd", NULL, bins = bins) 
     
     leaflet(ca_emp_map) %>%
       addTiles() %>%
       addPolygons(fillColor = ~pal(data), fillOpacity = 0.7, color = "white", highlight = highlightOptions(
         color = "blue", dashArray = "", fillOpacity = 0.7, bringToFront = T
       ), label = ~paste(area_title, data, sep = " : ")) %>%
       addLegend(pal = pal, values = ~data, opacity = 0.7, title = name, position = "topright")
     
   })
   
   output$table <- renderTable({
     dec_ca_emp <- ca_emp %>%
       filter(months(period) == input$select) 
     
     names(dec_ca_emp)[c(2, 3)] <- c("STATE", "COUNTY")
     
     #combine the sp data with the unemployment data
     us_counties$STATE <- as.numeric(us_counties$STATE)
     us_counties$COUNTY <- as.numeric(us_counties$COUNTY)
     
     ca_emp_map <- merge(us_counties, dec_ca_emp, by = c("STATE", "COUNTY"), all.x = F)
     
     
     data <- switch(input$item,
                    "Labor Force" = ca_emp_map$labor_force,
                    "Employed" = ca_emp_map$employed,
                    "Unemployed" = ca_emp_map$unemployed,
                    "Unemployed Rate" = ca_emp_map$unemployed_rate)
     
     df <- data.frame(area = ca_emp_map$area_title, data)
     df[order(df$data, decreasing = T), ]
   })
   
   output$summary <- renderPrint({
     dec_ca_emp <- ca_emp %>%
       filter(months(period) == input$select) 
     
     names(dec_ca_emp)[c(2, 3)] <- c("STATE", "COUNTY")
     
     #combine the sp data with the unemployment data
     us_counties$STATE <- as.numeric(us_counties$STATE)
     us_counties$COUNTY <- as.numeric(us_counties$COUNTY)
     
     ca_emp_map <- merge(us_counties, dec_ca_emp, by = c("STATE", "COUNTY"), all.x = F)
     
     data <- switch(input$item,
                    "Labor Force" = ca_emp_map$labor_force,
                    "Employed" = ca_emp_map$employed,
                    "Unemployed" = ca_emp_map$unemployed,
                    "Unemployed Rate" = ca_emp_map$unemployed_rate)
     
     summary(data)
   })
  
})
