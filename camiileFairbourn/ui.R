#Simulated Confidence Intervals from known population data
library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Simulated Confidence Intervals for the mean age of pregnant women"),
    sidebarLayout(
      sidebarPanel(
        numericInput("N","Sample Size",value=100,min=1),
        numericInput("conflev","Confidence Level (enter a value (Danny suggests: in percent) between 1 and 99.99)",value=95,min=1,max=100),
        hr(),
        tags$div(class="header", checked=NA,
                 tags$p("This application uses recent NHANES data about pregnant women 
                        in the United States. The average age of these women was 27.03 years."),
                 tags$p("The application will draw 100 samples of the size you specify
                        and visually represent the confidence interval based on each
                        sample. If the confidence interval does not include the true
                        mean of 27.03, the interval will be colored red."),
                 tags$p("Try different values for both the sample size and the confidence
                        level. Notice how the width of the intervals changes for different
                        sample sizes and confidence levels. Does a larger sample size give
                        narrower or wider intervals? Does a larger confidence level give
                        narrower or wider intervals?"),
                 tags$p("Beneath the confidence interval plot, the application
                        will construct a histogram of the sample averages, along with the
                        normal curve based on the expected value and standard error. Red 
                        vertical lines will indicate which sample means fell outside of
                        the indicated confidence level. ")

                 
        ),
        helpText(" ")
      ),
      mainPanel(
        plotOutput("ConfPlot"),
        plotOutput("SampMeanHist")
      )
    )
  )
)