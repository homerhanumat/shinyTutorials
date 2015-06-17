## Add tabsets

library(shiny)
library(scales) # for transparency in density-plot fill

## Set up underlying populations:
source("setup.R")

## Set upper limit on sims
simLimit <- 10000 #upper limit on number of sims at once

##########################################################
## ui
##########################################################

ui <- fluidPage(
  titlePanel('Exploring Confidence Intervals'),
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
    helpText("How many samples would you like to take at one time?  Limit is 10000. With each ",
             "sample, we'll make a confidence interval for the population mean."),
    numericInput("sims", "Number of Samples at Once", 1, min=0, max = simLimit, step=1),
    actionButton("takeSample","Sample Now"),
    conditionalPanel(
      condition = 'output.beginning == false',
      actionButton("reset","Start Over")
      )
    ), # end sidebarPanel
  mainPanel(
    conditionalPanel(
      condition = "input.takeSample == 0 || output.beginning == true",
      plotOutput("initialGraph"),
      HTML("<ul>
              <li>The population density curve is in red.</li>
              <li>The vertical line marks the population mean.</li>
            </ul>")
      ),
    conditionalPanel(
      condition = "output.beginning == false",
      tabsetPanel(
        tabPanel("Latest Interval",
                 plotOutput("plotSample"),
                 HTML("<p> </p>"),
                 HTML("<ul>
                        <li>The population density curve is in red.</li>
                        <li>The vertical line marks the population mean.</li>
                        <li>A density plot for the sample is in light blue.</li>
                        <li>The sample mean is the big blue dot.</li>
                        <li>The confidence interval is in green.</li>
                      </ul>"),
                br(''),
                tableOutput("summary")
                ),
        tabPanel("t-statistic",
                 plotOutput("tstatistic"),
                 HTML(
                   "<p>The plots above compare the actual distribution of the t-statistic to the t-curve with n-1 degrees of freedom.</p>
                    <p></p>
                    <ul>
                      <li>The t-curve is in red.  If the population is exactly normal, then this curve represents the exact distribution of the t-statistic.</li>
                      <li>The density plot of the t-statistics found so far is shown in blue.  This plot gives a pretty good estimate of the actual distribution of the t-statistic, for the population and sample size that you have selected.</li>
                    </ul>")
                 )
        ) # end tabset panel
      ) # end conditonal panel
    ) # end mainPanel
  )

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  ## set see so that users arelikely to get different results
  set.seed(as.numeric(Sys.time()))

  ## make an object that tracks the state of the app:
  rv <- reactiveValues(
    popDen = normalDen,
    popMean = normalMean,
    popMax = max(normalDen$x),
    popMin = min(normalDen$x),
    yMax = 1.5*max(normalDen$y),
    sample = NULL, 
    mean = NULL, 
    lower = NULL,
    upper = NULL,
    sims = 0,
    good = 0,
    begin = TRUE,
    tstats = numeric())
  
  observeEvent(input$popDist,
               {
                 rv$popDen <- switch(input$popDist,
                                     normal=normalDen,
                                     skew=skewDen,
                                     superskew=superSkewDen,
                                     outliers=outlierDen)
                 rv$popMean <- switch(input$popDist,
                                      normal=normalMean,
                                      skew=skewMean,
                                      superskew=superSkewMean,
                                      outliers=outlierMean)
                 rv$popMax <- switch(input$popDist,
                                     normal=max(normalDen$x),
                                     skew=max(skewDen$x),
                                     superskew=max(superSkewDen$x),
                                     outliers=max(outlierDen$x))
                 rv$popMin <- switch(input$popDist,
                                     normal=min(normalDen$x),
                                     skew=min(skewDen$x),
                                     superskew=min(superSkewDen$x),
                                     outliers=min(outlierDen$x))
                 rv$yMax <- switch(input$popDist,
                                   normal=1.5*max(normalDen$y),
                                   skew=1.5*max(skewDen$y),
                                   superskew=1.5*max(superSkewDen$y),
                                   outliers=1.5*max(outlierDen$y))
               }
               )
  
  observeEvent(input$takeSample, 
               {
                 # get the samples, make the intervals
                 n <- input$n
                 reps <- min(input$sims, simLimit)
                 # grab all the random items you need at once:
                 itemNumb <- reps*n
                 sampleItems <- switch(input$popDist,
                                       normal=rnorm(itemNumb,mean=muNorm,sd=sigmaNorm),
                                       skew=rgamma(itemNumb,shape=shapeGamma,scale=scaleGamma),
                                       superskew=rpareto(itemNumb,alpha=alphaPareto,theta=thetaPareto),
                                       outliers=routlier(itemNumb))
                 # arrange the random items in a matrix; the rows are your samples
                 sampleMatrix <- matrix(sampleItems,ncol=n,nrow=reps)
                 conf = input$confLevel/100
                 t.input = conf + ((1 - conf)/2)
                 tMultiplier = qt(t.input, df = n - 1)
                 # from the matrix, quickly compute the items you need
                 xbar <- rowSums(sampleMatrix)/n
                 se <- sqrt((rowSums(sampleMatrix^2)-n*xbar^2)/(n^2-n))
                 margin = tMultiplier * se
                 lower <- xbar - margin
                 upper <- xbar + margin
                 goodInterval <- ((rv$popMean > lower) & (rv$popMean < upper))
                 goodCount <- sum(goodInterval)
                 latestSamp <<- sampleMatrix[reps,]
                 # store in rv
                 rv$sample <- sampleMatrix[reps, ]
                 rv$mean <- xbar[reps]
                 rv$lower <- lower[reps]
                 rv$upper <- upper[reps]
                 rv$sims <- rv$sims + reps
                 rv$good <- rv$good + goodCount
                 rv$begin <- FALSE
                 rv$tstats <- c(rv$tstats, (xbar-rv$popMean)/se)
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
                 rv$tstats <- numeric()
               })
  
  output$beginning <- reactive({
    rv$begin
  })
  
  # needed for the conditional panels to work
  outputOptions(output, 'beginning', suspendWhenHidden=FALSE)
  
  output$initialGraph <- renderPlot({
    # the underlying population
    plot(rv$popDen$x,rv$popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlim=c(rv$popMin,rv$popMax),
         ylim=c(0,rv$yMax),
         xlab="",
         ylab="density")
    abline(v=rv$popMean,lwd=2)
  })
  
  output$plotSample <- renderPlot({
    # the underlying population
    plot(rv$popDen$x,rv$popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population, with Random Sample",
         xlim=c(rv$popMin,rv$popMax),
         ylim=c(0,rv$yMax),
         xlab="",
         ylab="density")
    abline(v=rv$popMean,lwd=2)
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
      intLevel <- 0.95*rv$yMax
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
  
  # produce the t-statistic plot
  output$tstatistic <- renderPlot({
    input$takeSample
    n <- input$n
    numberSims <- rv$sims
    tstats <- rv$tstats
    if (numberSims == 1) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw=1)
      }
    if (numberSims >= 2 && n < 5) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw=0.1)
      }
    if (numberSims >= 2 && n >= 5) {
      tstatDen <- density(tstats,n=1024,from=-10,to=10,bw="SJ")
      }
    if (numberSims > 0) {
      ymax <- max(tstatDen$y,dt(0,df=n-1))
      plot(tstatDen$x,tstatDen$y,type="l",lwd=2,col="blue",
           main="t-statistic vs. t-curve",cex.main=2,
           xlab="t", ylim=c(0,ymax),xlim=c(-6,6),
           ylab="density")
      curve(dt(x,df=n-1),-6,6,col="red",lwd=2,add=TRUE)
      } #end check that there are samples
  })
  } # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)