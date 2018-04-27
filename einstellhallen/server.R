#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  data <- reactive({
    dbinom(0:input$N,input$N,input$p/100)
    
  })
  
  output$barchart <- renderPlotly({
    
    n <- input$N
    p<- ((input$p) /100)
    x<- dbinom(0:n,size=n,prob=p)
    names(x)<-c("Kein Elektroautointeresse", "Einzelnes Elektroauto", "Mehr als ein Elektroauto")

    plot_ly(x=seq(0,n,by=1), y = x,text =list("Kein Elektroauto", "Einzelnes Elektroauto", "Mehr als ein Elektroauto"), textposition = 'auto', insidetextfont = list(color = '#FFFFFF'), type="bar", marker = list(color = c('rgba(31,119,180,1)','rgba(44,160,44,1)'))) %>%
      layout(title = "",
             barmode = 'group',
             xaxis = list(title = "Anzahl Elektroautos in Einstellhalle"),
             yaxis = list(title = "Häufigkeit"))
  })
  output$pie <- renderPlotly({
    n <- input$N
    p<- ((input$p) /100)
    x<- dbinom(0:n,size=n,prob=p)
    plot_ly(values=c(dbinom(0,size=n, prob=p),dbinom(1,size=n,prob=p),1-pbinom(1,size=n,prob=p)), labels=c("Kein Elektroauto", "Einzelnes Elektroauto", "Mehr als ein Elektroauto"), type="pie", insidetextfont = list(color = '#FFFFFF'), marker = list(colors = c('rgba(31,119,180,1)','rgba(44,160,44,1)','rgba(68,68,68,1)')))
   
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$summary <- renderPrint({
    summary(data())
  })
  
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame("Anzahl Elektroautos"=seq(0,input$N) ,Wahrscheinlichkeit=paste(round(data()*100,2),"%"))
  })
  
})

