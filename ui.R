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


# Define UI for application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Assignment 3: Problem 2"),
  
  # Sidebar with a slider input  
  
    sidebarPanel(
       selectInput("select", label = h4("Select month:"),
                   choices = as.character(unique(months(ca_emp$period))),
                   selected = 1),
       radioButtons("item", label = h4("Select statistics:"),
                    choices = c("Labor Force", "Employed", "Unemployed", "Unemployed Rate"),
                    selected = "Labor Force")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
               tabPanel("Plot", leafletOutput("plot")),
               tabPanel("Table", tableOutput("table")),
               tabPanel("Summary", verbatimTextOutput("summary"))
               )
    
  )
))
