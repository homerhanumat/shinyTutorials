## Add ability to choose population, and use conditional panels

library(shiny)
library(scales)

## Set up underlying populations:
source("setup.R")

##########################################################
## ui
##########################################################

ui <- fluidPage(
  
  titlePanel('What Does "Confidence Level" Mean?'),
  
  sidebarPanel(
    conditionalPanel(
      condition = "input.takeSample == 0 || output.beginning == true",
      selectInput(inputId="popDist",label="Population Shape",
                  choices=list("Normal"="normal",
                               "Skewy"="skew",
                               "REALLY Skewed"="superskew",
                               "Way-Out Outlier Group"="outliers")),
      br(),
      sliderInput(inputId="n","Sample Size n",value=2,min=2,max=50,step=1),
      helpText("How confident do you want to be that the population mean is contained",
             "within the confidence interval?   Use the slider to select a desired",
             "percent-confidence level."),
     sliderInput(inputId="confLevel","Confidence Level",value=80,min=50,max=99,step=1)
    ),
    
  actionButton("takeSample","Sample Now"),
    
  conditionalPanel(
      condition = 'output.beginning == false',
      actionButton("reset","Start Over")
  )
    
  ), # end sidebarPanel
  
  mainPanel(
    
    plotOutput("plotSample"),
    conditionalPanel(
        condition = 'output.beginning == false',
        tableOutput("summary")
    )
    
  )  # end mainPanel
  
)

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  ## set see so that users arelikely to get different results
  set.seed(as.numeric(Sys.time()))
  
  rvPop  <- reactiveValues(
    popDen = normalDen,
    popMean = normalMean,
    popMax = max(normalDen$x),
    popMin = min(normalDen$x)
  )
  
  observeEvent(input$popDist,
               {
    rvPop$popDen <- switch(input$popDist,
           normal=normalDen,
           skew=skewDen,
           superskew=superSkewDen,
           outliers=outlierDen)
    rvPop$popMean <- switch(input$popDist,
           normal=normalMean,
           skew=skewMean,
           superskew=superSkewMean,
           outliers=outlierMean)
    rvPop$popMax <- switch(input$popDist,
           normal=max(normalDen$x),
           skew=max(skewDen$x),
           superskew=max(superSkewDen$x),
           outliers=max(outlierDen$x))
    rvPop$popMin <- switch(input$popDist,
           normal=min(normalDen$x),
           skew=min(skewDen$x),
           superskew=min(superSkewDen$x),
           outliers=min(outlierDen$x))
               }
  )
  
  
  yMax <- reactive({
    max(rvPop$popDen$y)*1.5
  })
  
  rv <- reactiveValues(sample = NULL, 
                       mean = NULL, 
                       lower = NULL,
                       upper = NULL,
                       sims = 0,
                       good = 0,
                       begin = TRUE)
  
  observeEvent(input$takeSample, 
               {
                 # random sample and its mean
                 n <- input$n
                 samp <- switch(input$popDist,
                            normal=rnorm(n,mean=muNorm,sd=sigmaNorm),
                            skew=rgamma(n,shape=shapeGamma,scale=scaleGamma),
                            superskew=rpareto(n,alpha=alphaPareto,theta=thetaPareto),
                            outliers=routlier(n))
                 xbar <-  mean(samp)
                 
                 # make bounds for the confidence interval
                 conf  <- isolate(input$confLevel/100)
                 t.input <- conf + ((1 - conf)/2)
                 tMultiplier <- qt(t.input, df = input$n - 1)
                 se <-  sd(samp)/sqrt(input$n)
                 margin <- tMultiplier * se
                 lower <- xbar - margin
                 upper <- xbar + margin
                 
                 # does the interval contain the parameter?
                 goodInterval <- rvPop$popMean >= lower & rvPop$popMean <= upper
                 
                 # store in rv
                 rv$sample <- samp
                 rv$mean <- xbar
                 rv$lower <- lower
                 rv$upper <- upper
                 rv$sims <- rv$sims + 1
                 rv$good <- rv$good + goodInterval
                 rv$begin <- FALSE
                 })
  
  observeEvent(input$reset,
               {
                 rv$sample <- NULL
                 rv$mean <- NULL
                 rv$lower <- NULL
                 rv$upper <- NULL
                 rv$sims <- 0
                 rv$good <- 0
                 rv$begin <- TRUE
               })
  
  output$beginning <- reactive({
    rv$begin
  })
  
  # needed for the conditional panels to work
  outputOptions(output, 'beginning', suspendWhenHidden=FALSE)
  
  output$plotSample <- renderPlot({
    # the underlying population
    plot(rvPop$popDen$x,rvPop$popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlim=c(rvPop$popMin,rvPop$popMax),
         ylim=c(0,yMax()),
         xlab="",
         ylab="density")
    abline(v=rvPop$popMean,lwd=2)
    
    # sample and interval
    if (! rv$begin) {

      # density plot for the sample
      sampDen <- density(rv$sample, from = 0)
      xdens <- sampDen$x
      ydens <- sampDen$y
      firstx <- xdens[1]
      lastx <- xdens[length(xdens)]
      polygon(x = c(firstx,xdens,lastx), y = c(0,ydens,0), col = alpha("lightblue",0.5))
      
      # now the interval
      intLevel <- 0.95*yMax()
      segments(x0 = rv$lower, y0 = intLevel, x1 = rv$upper, y1 = intLevel, 
               col = "green", lwd = 3)
      text(x=rv$lower,y=intLevel,labels="(")
      text(x=rv$upper,y=intLevel,labels=")")
      points(rv$mean, intLevel, col = "blue", pch = 20,cex=2)
      rug(rv$sample)
    }
    
  })  # end plotSample
  
  # summary of intervals so far
  output$summary <- renderTable({
     df <- data.frame(rv$sims,
                      rv$good,
                      ifelse(rv$sims >0, round(rv$good/rv$sims*100,3), NA))
    names(df) <- c("Simulations", "Good Intervals", "Percentage Good")
    df
  }, include.rownames = FALSE)
  
} # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)