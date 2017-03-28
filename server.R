
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {
  
  rv_cars <- reactiveVal()
  
  observeEvent(input$cgi_car_names, {
    rv_cars(input$cgi_car_names)
  })
  
  cars <- reactive({
    invalidateLater(2000, session)
    filt <- round(runif(1, 12, 30))
    mtcars %>% 
      dplyr::mutate(nm = row.names(mtcars)) %>% 
      dplyr::filter(mpg > filt)
  })
  
  car_names <- reactive({
    cars()$nm
  })

  output$distPlot <- renderPlot({
    
    filtered <- cars()
    ggplot(filtered, aes(cyl, qsec)) + 
      geom_col(fill = input$rbChoices)

  })
  
  output$cgiCarNames <- renderUI({
    choices <- car_names()
    selected <- if (length(rv_cars()) == 0) NULL else {
      rv_cars()
    }
    
    checkboxGroupInput("cgi_car_names", label = "Car names", choices = choices, selected = selected)
  })

})
