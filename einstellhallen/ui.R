#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjqui)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Simulation der Elektroautoanzahl in Einstellhallen"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("p", "Anteil Elektroautos in der Schweiz in Prozent:", min=0, max=50, value=0.3, step = 0.1, animate = animationOptions(interval = 300, loop = TRUE)),
      paste("Marktanteil 2017: 0.3%","Neuzulassungen 2017: 1.7%","Prognose 2030 über 10%", sep="\n"),
      sliderInput("N", "Anzahl Parkfelder in Einstellhalle", min=2, max=60, value=5)
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Grafiken", plotlyOutput("barchart"),plotlyOutput("pie")), 
        #tabPanel("Details", verbatimTextOutput("summary")), 
        tabPanel("Tabelle", tableOutput("table"))
      )
    )
  )
))

