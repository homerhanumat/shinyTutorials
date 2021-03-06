## Add ability to set the number of simulations to perform

library(shiny)
library(scales) # for transparency in density-plot fill

## Set up the underlying populations for the app


########################################
#  Utility Functions for Generating Pareto Values
########################################

rpareto <- function(n,alpha,theta) {#random values for Pareto(alpha,theta) distribution
  theta*((1-runif(n))^(-1/alpha)-1)
}

dpareto <- function(x,alpha,theta) {  #pdf for Pareto(alpha,theta) distribution
  alpha*theta^alpha/(x+theta)^(alpha+1)
}


#############################################
# Generate the populations
############################################
muNorm <- 70
sigmaNorm <- 5
shapeGamma <- 2
scaleGamma <- 50

# for pareto:
alphaPareto <- 5
thetaPareto <- 100
tailProb <- 0.02  #want to find a Value at risk of 1 - this
valRisk <- thetaPareto*(tailProb^(-.5)-1)

# for pop with group of outliers
propOutliers <- 0.10
meanOutliers <- 200
sdOutliers <- 5
meanRegulars <- 50
sdRegulars <- 5

routlier <- function(n) {
  propNormals <- 1- propOutliers
  whichHump <- rbinom(n,size=1,prob=propNormals)
  outlierSamp <- ifelse(whichHump,rnorm(n,mean=meanRegulars,sd=sdRegulars),
                        rnorm(n,mean=meanOutliers,sd=sdOutliers))
  outlierSamp
}

######################################
# Make population densities
#####################################
xNorm <- seq(muNorm-5*sigmaNorm,muNorm+5*sigmaNorm,length.out=600)
yNorm <- dnorm(xNorm,mean=muNorm,sd=sigmaNorm)
normalDen <- list(x=xNorm,y=yNorm)

xSkew <- seq(0,shapeGamma*scaleGamma+7.5*sqrt(shapeGamma)*scaleGamma,
             length.out=600)
ySkew <- dgamma(xSkew,shape=shapeGamma,scale=scaleGamma)
skewDen <- list(x=xSkew,y=ySkew)

xSuperSkew <- seq(0,valRisk,length.out=600)
ySuperSkew <- dpareto(xSuperSkew,alpha=alphaPareto,theta=thetaPareto)
superSkewDen <- list(x=xSuperSkew,y=ySuperSkew)

xOut <- seq(0,meanOutliers+5*sdOutliers,length.out=600)
yOut <- (1-propOutliers)*dnorm(xOut,mean=meanRegulars,sd=sdRegulars)+propOutliers*dnorm(xOut,mean=meanOutliers,sd=sdOutliers)
outlierDen <- list(x=xOut,y=yOut)

#######################################
# Get the population means
######################################

normalMean <- muNorm
skewMean <- shapeGamma*scaleGamma
superSkewMean <- thetaPareto/(alphaPareto - 1)
outlierMean <- (1-propOutliers)*meanRegulars+propOutliers*meanOutliers

## Set upper limit on sims
simLimit <- 10000 #upper limit on number of sims at once

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
      condition = "output.beginning == true",
      plotOutput("initialGraph")
    ),
    conditionalPanel(
      condition = 'output.beginning == false',
      plotOutput("plotSample"),
      HTML("<ul>
                <li>The population density curve is in red.</li>
                <li>The vertical line marks the population mean.</li>
                <li>A density plot for the most recent sample is in light blue.</li>
                <li>The most recent sample mean is the big blue dot.</li>
                <li>The most recent confidence interval is in green.</li>
              </ul>"),
      br(''),
      tableOutput("summary")
    )
  ) # end MainPanel
  )

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  ## set see so that users arelikely to get different results
  set.seed(as.numeric(Sys.time()))
  ## make object to track the state of the app:
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
    begin = TRUE)
  
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
  
} # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)