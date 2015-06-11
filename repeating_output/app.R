library(shiny)
library(tigerstats)

##########################################################
## ui
##########################################################

ui <- fluidPage(
  
  titlePanel('Do Not Repeat Your Output'),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel(title = "Panel 1",
        tableOutput("crosstab")
        ),
      tabPanel(title = "Panel 2",
          tableOutput("crosstab")
        )
    )
  )  # end mainPanel
  
) # end fluid page

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  output$crosstab <- renderTable({
    xtabs(~weather+ crowd.behavior, data = ledgejump)
  })
  
#   output$crosstab2 <- renderTable({
#     xtabs(~weather+ crowd.behavior, data = ledgejump)
#   })
  
} # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)