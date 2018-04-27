#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(lubridate)
library(dplyr)
library(RColorBrewer)
profile<-read.csv("badstrasse.csv",  header=TRUE,stringsAsFactors = FALSE)
profile$Zeit<-as.POSIXct(profile$Zeit, format = "%d.%m.%Y %H:%M")
profile$hour <- hour(profile$Zeit)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lastganganalyse"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("start",
                     "Anfang:",
                     min = 0,
                     max = 23,
                     value = 22),
         sliderInput("stop",
                     "Ende:",
                     min = 0,
                     max = 23,
                     value = 6),
         selectInput("fuse", "Hausanschlusssicherung:",
                     choices= c(9,20, 25, 32, 40, 50, 63, 80, 100, 125), selected=40)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          
        tabPanel("3D Scatter Plot", plotlyOutput("distPlot", height="auto")),
        tabPanel("Zeitübersicht", plotOutput("clocks"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
     if (input$start > input$stop) {
       draw<-subset(profile,profile$hour>=input$start | profile$hour<input$stop)
     }else{
       draw<-subset(profile,profile$hour>=input$start & profile$hour<input$stop)
     }
     
     plot_ly(x = draw$I1max, y = draw$I2max, z = draw$I3max,color = draw$hour, colors=c('#d8b365','#f5f5f5','#5ab4ac')) %>%
       add_markers() %>%
       layout(scene = list(xaxis = list(title = 'L1', range=c(0,input$fuse)),yaxis = list(title = 'L2',range=c(0,input$fuse)),zaxis = list(title = 'L3',range=c(0,input$fuse)))) 
     
   })
   # Clock plot function
   clock.plot <- function (x, col = ifelse(x > 0.5,ifelse(x>1,'red','grey'),'lightgreen'), ...) {
     if( min(x)<0 ) x <- x - min(x)
     if( max(x)>1 ) x <- x/as.numeric(input$fuse)
     n <- length(x)
     if(is.null(names(x))) names(x) <- 0:(n-1)
     m <- 1.05
     plot(0, type = 'n', xlim = c(-m,m), ylim = c(-m,m), axes = F, xlab = '', ylab = '', ...)
     a <- pi/2 - 2*pi/200*0:200
     polygon( cos(a), sin(a) )
     v <- .02
     a <- pi/2 - 2*pi/n*0:n
     segments( (1+v)*cos(a), (1+v)*sin(a), (1-v)*cos(a), (1-v)*sin(a) )
     segments( cos(a), sin(a),0, 0, col = 'light grey', lty = 3) 
     ca <- -2*pi/n*(0:50)/50
     for (i in 1:n) {
       a <- pi/2 - 2*pi/n*(i-1)
       b <- pi/2 - 2*pi/n*i
       polygon( c(0, x[i]*cos(a+ca), 0), c(0, x[i]*sin(a+ca), 0), col=col[i] )
       v <- .1
       text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
     }
   }
   
   output$clocks <- renderPlot({
     

     
     # Use the function on the created data
     # 
     values <- data.frame(profile %>% group_by(hour) %>% select(I1max,I2max ,I3max) %>% summarize(max(I1max),max(I2max),max(I3max),mean(I1max),mean(I2max),mean(I3max)))
     par(mfrow=c(2,3)) 
     clock.plot(values$max.I1max,main = "L1 Maximum")
     clock.plot(values$max.I2max,main = "L2 Maximum")
     clock.plot(values$max.I3max,main = "L3 Maximum")
     clock.plot(values$mean.I1max,main = "L1 Mittelwert")
     clock.plot(values$mean.I2max,main = "L2 Mittelwert")
     clock.plot(values$mean.I3max,main = "L3 Mittelwert")
     # # 
     # 
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

