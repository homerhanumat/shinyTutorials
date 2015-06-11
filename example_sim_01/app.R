## Initial App

library(shiny)
library(scales)

############################################################
## Set up the gamma-distributed population
###########################################################
shapeGamma <- 2
scaleGamma <- 50
xSkew <- seq(0,shapeGamma*scaleGamma+7.5*sqrt(shapeGamma)*scaleGamma,
             length.out=600)
ySkew <- dgamma(xSkew,shape=shapeGamma,scale=scaleGamma)
popDen <- list(x=xSkew,y=ySkew)
popMean <- shapeGamma*scaleGamma
yMax <- 1.5*max(popDen$y)

############################################################
## ui
############################################################

ui <- fluidPage(
  
  titlePanel('What Does "Confidence Level"Mean?'),
  
  sidebarPanel(
    sliderInput(inputId="n","Sample Size n",value=2,min=2,max=50,step=1),
    helpText("How confident do you want to be that the population mean is contained",
             "within the confidence interval?   Use the slider to select a desired",
             "percent-confidence level."),
    
    sliderInput(inputId="confLevel","Confidence Level",value=80,min=50,max=99,step=1),
    actionButton("takeSample","Sample Now")
  ), # end sidebarPanel
  
  mainPanel(
    
    plotOutput("plotSample")
    
  )  # end mainPanel
  
)

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  ## set see so that users arelikely to get different results
  set.seed(as.numeric(Sys.time()))
  
  rv <- reactiveValues(sample = NULL, 
                       mean = NULL, 
                       lower = NULL,
                       upper = NULL)
  
  observeEvent(input$takeSample, 
               {
                 # random sample and its mean
                 samp <- rgamma(input$n,shape=shapeGamma,scale=scaleGamma)
                 xbar <-  mean(samp)
                 
                 # make bounds for the confidence interval
                 conf = isolate(input$confLevel/100)
                 t.input = conf + ((1 - conf)/2)
                 tMultiplier = qt(t.input, df = input$n - 1)
                 se = sd(samp)/sqrt(input$n)
                 margin = tMultiplier * se
                 
                 # store in rv
                 rv$sample <- rgamma(input$n,shape=shapeGamma,scale=scaleGamma)
                 rv$mean <- xbar
                 rv$lower <- xbar - margin
                 rv$upper <- xbar + margin
                 })
  
  
  output$plotSample <- renderPlot({
    
    # the underlying population
    plot(popDen$x,popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlab="",
         ylab="density",
         ylim = c(0,yMax))
    abline(v=popMean,lwd=2)
    
    # sample and interval
    if (input$takeSample) {
      hist(rv$sample, freq = FALSE, col = alpha("lightblue", 0.5), add = T)

      # now the interval
      intLevel <- 0.95*yMax
      segments(x0 = rv$lower, y0 = intLevel, x1 = rv$upper, y1 = intLevel, 
               col = "green", lwd = 3)
      text(x=rv$lower,y=intLevel,labels="(")
      text(x=rv$upper,y=intLevel,labels=")")
      points(rv$mean, intLevel, col = "blue", pch = 20,cex=2)
      rug(rv$sample)
    }
    
  })  # end plotSample
  
} # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)