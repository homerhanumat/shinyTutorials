library(shiny)

##########################################################
## ui
##########################################################

ui <- fluidPage(
  
  titlePanel('DRY Bilocation'),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel(title = "Panel 1",
        tableOutput("crosstab")
        ),
      tabPanel(title = "Panel 2",
          tableOutput("crosstab2")
        )
    )
  )  # end mainPanel
  
) # end fluid page

#################################################################
## server
#################################################################
  
  server <- function(input, output) {
    
    makeTable <- function() {
      xtabs(~ am + cyl, data = mtcars)
    }
    
    output$crosstab <- renderTable({
      makeTable()
    })
    
    output$crosstab2 <- renderTable({
      makeTable()
    })
  
} # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)