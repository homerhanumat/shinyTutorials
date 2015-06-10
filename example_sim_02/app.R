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
    
    plotOutput("plotSample"),
    tableOutput("summary")
    
  )  # end mainPanel
  
)

#################################################################
## server
#################################################################

server <- function(input, output) {
  
  rv <- reactiveValues(sample = NULL, 
                       mean = NULL, 
                       lower = NULL,
                       upper = NULL,
                       sims = 0,
                       good = 0)
  
  observeEvent(input$takeSample, 
               {
                 # random sample and its mean
                 samp <- rgamma(input$n,shape=shapeGamma,scale=scaleGamma)
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
                 goodInterval <- popMean >= lower & popMean <= upper
                 
                 # store in rv
                 rv$sample <- rgamma(input$n,shape = shapeGamma,scale = scaleGamma)
                 rv$mean <- xbar
                 rv$lower <- lower
                 rv$upper <- upper
                 rv$sims <- rv$sims + 1
                 rv$good <- rv$good + goodInterval
                 })
  
  
  output$plotSample <- renderPlot({
    
    # the underlying population
    plot(popDen$x,popDen$y,type="l",lwd=3,col="red",
         main="Density Curve of Population",
         xlab="",
         ylab="density")
    abline(v=popMean,lwd=2)
    
    # sample and interval
    if (input$takeSample) {
      hist(rv$sample, freq = FALSE, col = alpha("lightblue", 0.5), add = T)
      segments(x0 = rv$lower, y0 = 0, x1 = rv$upper, y1 = 0, 
               col = "green", lwd = 3)
      text(x=rv$lower,y=0,labels="(")
      text(x=rv$upper,y=0,labels=")")
      points(rv$mean, 0, col = "blue", pch = 20,cex=2)
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