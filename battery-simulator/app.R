#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape)
library(plotly)
library(shinythemes)
library(formattable)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("Welche Autobatterie passt zu mir?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("km",
                  "Durchschnittliche km pro Tag:",
                  min = 10,
                  max = 300,
                  value = 50),
      sliderInput("stops",
                  "Ladestops pro Woche:",
                  min = 1,
                  max = 21,
                  value = 7),
      sliderInput("duration",
                  "Stunden pro Ladestop:",
                  min = 1,
                  max = 14,
                  value = 8),
      selectInput("choice", "Auswahl nach", c( "Brutto Batteriekapazität (100%)"="1","Batteriekapazität (80%)"="0.8","Reichweite bei extremen Wetterbedingungen (60%)"="0.6"), selected = "0.8", multiple = FALSE,selectize = TRUE),
      tableOutput("values")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Table summarizing the values entered ----
      
      plotlyOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$values <- renderTable({
   sliderValues()
  })
  sliderValues <- reactive({
    bat_min <- ((((input$km*7)/input$stops)*0.22)/as.numeric(input$choice))
    
    data<- data.frame(
      
      akku = c(round(bat_min,0)),
      leistung = c(round(bat_min/input$duration,1)),
      reichweite = c(round((bat_min/0.22)*0.8,0)),
      kalt = c(round((bat_min/0.22)*0.6,0))
    )
    colnames(data)=c("Minimale Akkugrösse [kWh]",                        "Minimale Ladeleistung [kW]",                        "Reichweite [km]",                        "Reichweite bei -20/+35°C [km]")
    ladewarnung <- formatter("span", 
                             style = x ~ style(color = ifelse(x < 3.6, "green", 
                                                              ifelse(x > 11, "red", "black"))))
    #melt(data)
    formattable(data)
  })    
  output$distPlot <- renderPlotly({
    
    capacity = seq(20,100,10)
    performance = seq(150,250,10)
    dist_min = input$km *7 /input$stops / as.numeric(input$choice)
    margin = 0.5
    dist_with_margin = dist_min * (1+margin)
    
    a <- matrix(rep(1, times=length(capacity)*length(performance)), nrow=length(capacity), ncol=length(performance))
    colnames(a) <- as.character(performance)
    rownames(a) <- as.character(capacity)
    
    for (i in 0:length(capacity)){
      a[i,]<-a[i,]*capacity[i]*1000
    }
    
    for (i in 0:length(performance)){
      a[,i]<-round(a[,i]/performance[i],0)
    }
 
    
    
    plot_ly(
      x=performance, 
      y=capacity,
      z=a,
      type="heatmap", 
      zauto = FALSE,
      zmin = dist_min, 
      zmax = dist_with_margin, 
      reversescale =FALSE, 
      hoverinfo='y+z', 
      colorbar =list(tickvals=c(dist_min, dist_with_margin), tickmode="array",ticktext=c("zuwenig Reichweite","genügend Reichweite"))
            
            ) %>%
      layout(xaxis = list(title="Fahrweise in kW/km", showcrossline=TRUE), yaxis = list(title="Batteriekapazität in kWh"),xaxis2 = list(
        
        overlaying = "x", #important! as the xaxis1 should be the main one						 				
        side = "top",
        title = 'Fahrweise',
        #	range = c(xmin,xmax),
        #	zeroline = TRUE,
        #	#autotick = FALSE,
        tickmode = "array",
        tickvals=c(0,2,4,6),
        tickangle = 0,
        ticktext=c("Extrem Sparsam","Normal","Sportlich","Sehr heiss/kalt" )
        
      ),margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

