library(shiny)

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
  )
)
  
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
  
}

shinyApp(ui = ui, server = server)