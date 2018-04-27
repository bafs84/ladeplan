#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(plotly)

# station_data <- read.csv(file="meteo_exp.csv", sep=";", header=FALSE, col.names=c("station","time","temp"),colClasses = c('character', NA, 'numeric'),na.strings='NULL')
# station_data <-melt(station_data,id=c("time","temp"))
# station_data <-dcast(station_data, time ~ value, value.var = 'temp')
# station_data$time <- as.POSIXlt(station_data$time)
# station_data<-subset(station_data, station_data$time >= as.POSIXlt('2014-01-01 00:00:00') & station_data$time < as.POSIXlt('2018-01-01 00:00:00'))
station_data <- readRDS(file="data.Rda")
abs_min <- -30
abs_max <- 40
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Wettereinfluss auf Elektroautos"),
   
   # Sidebar with a slider input for number of bins 
   
   fluidRow(
     column(4,selectInput("select", "Wetterstation wählen:", tail(names(station_data),-1), multiple = FALSE)),
     column(4,sliderInput("range", "Temperaturbereich:", min = abs_min, max = abs_max, value = c(0,30))),
     column(4, tags$strong("Beobachtungen:"),textOutput("selected_var"))
   ),
        #sliderInput("range", "Uhrzeit:",min = 0, max = 23,value = c(0,23))
   plotlyOutput("distPlot",width="100%", height="800px"),
   tableOutput('table')
      
      # Show a plot of the generated distribution
      
   
)

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
  myfunction <- function(var){
    var = (var*9/5)+32
    var = ((0.3950-0.0022*var+9.1978*10^(-5)*var^2-3.9249*10^(-6)*var^3+5.2918*10^(-8)*var^4-2.0659*10^(-10)*var^5 )*1000*0.62137119223733)
    return (1-(var/166.2169))
  }
  observeEvent(input$select, {
    dt <- data.frame(time=station_data$time , temp=station_data[,input$select])
    
    dt <- na.omit(dt)
    dt <-data.frame(x=as.numeric(dt$time), y=dt$temp)
    dens <- density(dt$y)
    df <- data.frame(x=dens$x, y=dens$y)
    probs <- c(0.001, 0.01, 0.05, 0.95,0.99,0.999)
    quantiles <- quantile(dt$y, prob=probs)
    updateSliderInput(session, "range",value = c(unname(round(quantiles[3],0)), unname(round(quantiles[4],0))))
  })

  output$selected_var <- renderText({ 
    dt <- station_data[,input$select]
    dt <- na.omit(dt)
    kalt<- sum(dt<min(input$range))/length(dt)
    warm<- sum(dt>max(input$range))/length(dt)
    paste("Der Temperaturbereich deckt ungefähr ",100-round((kalt+warm)*100,0),"% der Stunden ab.\n In etwa ", round(kalt*100,0),"% war es kälter, in ", round(warm*100,0),"% noch wärmer")
  })  
  
   output$distPlot <- renderPlotly({
     if (!is.null(input$select)) {
       
     
     dt <- data.frame(time=station_data$time , temp=station_data[,input$select])
     
     dt <- na.omit(dt)
     dt <-data.frame(x=as.numeric(dt$time), y=dt$temp)
     dens <- density(dt$y)
     df <- data.frame(x=dens$x, y=dens$y)
     probs <- c(0.001, 0.01, 0.05, 0.95,0.99,0.999)
     quantiles <- quantile(dt$y, prob=probs)
     df$quant <- factor(findInterval(df$x,quantiles))
     p1 <- ggplot(df, aes(x,y)) + geom_line() + theme_light()+geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(limits = c(-30, 40),breaks=quantiles, labels=paste(round(quantiles,0),"°C\n",probs*100,"%")) + scale_fill_brewer(guide="none") +theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
     #+  labs(title = "Temperaturverteilungsfunktion", subtitle = paste("Messstation ",input$select),x="Temperatur und Quantil",caption="Stundendaten aus den Jahren 2014,2015,2016,2017" ) 
     
     
     
     
     min_temp <- min(dt$y)
     max_temp <- max(dt$y)
     temp <- seq(min_temp,max_temp,1)
     watt <- sapply(temp,myfunction)
     percentage <- sapply(seq(abs_min,abs_max,1),myfunction)
     data2 <- data.frame(temp,"30kWh" = watt)
     test <- melt(data2, id="temp")
     
     p2 <- ggplot(test, aes(x=temp, y=value, colour=variable)) + 
       geom_line(size=2)+ 
       theme_light() + 
       theme(legend.position="none") + 
       scale_colour_brewer( direction=-1) +
       scale_x_continuous(limits = c(abs_min, abs_max)) +
       #scale_y_continuous(limits = c(0, -0.5)) +
       #geom_text_repel(data = subset(test, temp == max(temp)), aes(label = c("30kWh","50kWh","70kWh")),size = 4, nudge_x = 45, segment.color = NA) +
       annotate("text", x = input$range+2, y = -0.5 , label = paste(as.character(round(percentage[input$range-abs_min]*100,0)),"%")) + 
       geom_vline(data = data.frame(input$range) , aes(xintercept = input$range),linetype="dotted")
       #+labs(title = "Reichweite nach Temperaturabhängigkeit", subtitle = paste(input$select," Temperaturbereich :",min_temp,"°C bis",max_temp,"°C" ), caption = "Quelle: Environ. Sci. Technol.  49, 6, 3974-3980", x = "Temperatur in C°", y="Reichweite in km")
     
      #grid.arrange(p1, p2, ncol=1)
      subplot(p1,p2,nrows=2,margin=0.1)

     } else {
       p("Bitte Wetterstation auswählen")
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

