
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output, session) {
  
  selected_cars <- reactiveVal()
  
  observeEvent(input$cgiNames, {
    cat("###################", "Event", "###################\n", sep = '\n')
    
    available <- cars()$nm
    old_selection  <- selected_cars()
    new_selection <- input$cgiNames
    
    to_keep <- setdiff(old_selection, available)
    new_rv <- c(new_selection, to_keep)
    
    
    cat('available: ', toString(available), '\n')
    cat('old_selection: ', toString(old_selection), '\n')
    cat('new_selection: ', toString(new_selection), '\n')
    cat('to_keep: ', toString(to_keep), '\n')
    cat('new_rv: ', toString(new_rv), '\n\n\n')
    
    selected_cars(new_rv)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observe({
    cat("observe\n")
    x <- cars()$nm
    y <- selected_cars()[selected_cars() %in% x]
    print(y)
    updateCheckboxGroupInput(session, "cgiNames", choices = x, selected = y)
  })
  
  cars <- reactive({
    invalidateLater(2000, session)
    filt <- round(runif(1, 12, 30))
    mtcars %>% 
      dplyr::mutate(nm = row.names(mtcars)) %>% 
      dplyr::filter(mpg > filt)
  })
  
  output$distPlot <- renderPlot({
    ggplot(cars(), aes(cyl, qsec)) + 
      geom_col(fill = input$rbChoices)
  })


})
