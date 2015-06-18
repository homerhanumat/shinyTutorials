library(shiny)

ui <- fluidPage(
  titlePanel('Sorry, No Bilocation!'),
  mainPanel(
    tabsetPanel(
      tabPanel(title = "Panel 1",
               tableOutput("crosstab")
      ),
      tabPanel(title = "Panel 2",
               tableOutput("crosstab")
      )
    )
  )
)

server <- function(input, output) {
  
  output$crosstab <- renderTable({
    xtabs(~ am + cyl, data = mtcars)
  })
  
}

shinyApp(ui = ui, server = server)