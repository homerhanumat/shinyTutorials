#Simulated Confidence Intervals
library(shiny)
dat<-read.csv("http://www.math.usu.edu/cfairbourn/Stat2300/RStudioFiles/data/preg.csv",header=TRUE)
age<-dat$age
mu<-mean(age)
reps<-100
res <- NULL
Q <- NULL
# This is to keep the samples the same for each student, but let them differ
# between students
random_seed <- as.numeric(Sys.time())

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  output$ConfPlot<-renderPlot({
    N<-input$N
    Q<<-abs(qnorm((100-input$conflev)/200))
    res<<-array(0,dim=c(reps,3)) 
    set.seed(random_seed)
    for(i in 1:reps) {y<-sample(age,N)
                      res[i,1]<-mean(y)
                      res[i,2]<-sd(y)
                      res[i,3]<-sd(y)/sqrt(N)} 
    plot(mu + c(-5,5),c(1,1),type="n",xlab="Age",
         ylab="Intervals",ylim=c(1,100))
    abline(v=mean(age))
    
    for(i in 1:reps){
      interval<-c(res[i,1]-Q*res[i,3],res[i,1]+Q*res[i,3])
      color<-ifelse(interval[1]<=mean(age) & 
                      interval[2]>=mean(age),1,2)
      lines(interval, c(i,i),col=color)
    } 
    
    
  })
  output$SampMeanHist <- renderPlot({
    
    # Render a histogram of 100 sample means
    
    hist(res[,1],prob=TRUE,main=paste("Histogram of",reps," sample averages"),
         xlab="Sample average",ylab="Proportion per sample average",
         xlim=c(22,32))
    points(seq(min(age), max(age), length.out=500),
           dnorm(seq(min(age), max(age), length.out=500),
                 mean(age), sd(age/sqrt(input$N))), type="l", col="darkblue", lwd=2)
    intmin<-mean(age)-Q*sd(age/sqrt(input$N))
    intmax<-mean(age)+Q*sd(age/sqrt(input$N))
    abline(v=intmin,col="red",lwd=2)
    abline(v=intmax,col="red",lwd=2) 
    
  })
})

