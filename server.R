library(shiny)
library(ggplot2)
library(reshape2)
library(gridExtra)


shinyServer(function(input, output){
  
  data <- reactive({inFile <- input$file1
  
                    if (is.null(inFile))
                    return(NULL)
                    print(inFile$datapath)
                    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
                    #read.csv("GraphPad Course Data/diseaseX.csv")
                    
                    if(!input$factors) data <- melt(data)
                    colnames(data) <- c("variable","value")
                    data
  })
  
#  output$plot <- renderPlot({
#    plot(data(), xlab="X", ylab="Y", ylim=c(-300,800))
#    if(input$line) {
#      abline(lm(Y ~ X, data=data()), col="dark blue")
#    }
#    if(input$means) {
#      abline(v = mean(data()[,1]), lty="dotted")
#      abline(h = mean(data()[,2]), lty="dotted")
#    } 
#    if(input$ant) {
#      model = lm(Y ~ X, data=data())
#      txt = paste("The equation of the line is:\nY = ",
#                  round(coefficients(model)[1],0)," + ",
#                  round(coefficients(model)[2],3),"X + error")
      
#      boxed.labels(50,600,labels=txt,bg="white", cex=1.25)
#    }    
    
#  })
 #
  
  output$mytable= renderDataTable({
    df <- data()
    df
  }
  )
  

  
  output$histogram<- reactivePlot(function(){
    
  df <- data()
  
  #datacol1 <- as.numeric(input$dataCol1)
  #datacol2 <- as.numeric(input$dataCol2)
  
  #mdf <- melt(df[,c(datacol1,datacol2)])
  p <- ggplot(df, aes(x = value, fill=variable)) + geom_density(alpha=0.3)

  print(p)
  }
  )
  
  output$boxplot<- reactivePlot(function(){
    
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
   #mdf <- melt(df[,c(datacol1,datacol2)])
    
    p <- ggplot(df, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()
    
    print(p)

    
  }
  )
  

  
  output$ttest <-renderPrint({
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
    
    #X <- df[,datacol1]
    #Y <- df[,datacol2]
    alternative = input$alternative
    paired <- as.logical(input$paired)
    var.equal <- as.logical(input$var.equal)    
    t.test(value~variable,data=df,alternative=alternative,paired=paired,var.equal=var.equal)
  })

  output$summary <- renderPrint({
    df <- data()
    lapply(split(df$value,df$variable),summary)
  })
  
  output$tdist <- reactivePlot(function(){
  
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
    #X <- df[,datacol1]
    #Y <- df[,datacol2]
    alternative = input$alternative
    paired <- as.logical(input$paired)
    var.equal <- as.logical(input$var.equal)    
    tt <- t.test(value~variable,data=df,alternative=alternative,paired=paired,var.equal=var.equal)

    tstat <- tt$statistic
    degfree <- tt$parameter

    alternative = input$alternative
    
    df <- data.frame(ts = rt(10000,df=degfree))

    
    p<- ggplot(df, aes(x=ts)) + 
      geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     binwidth=.5,
                     colour="black", fill="white") +
      geom_density()
    
   xlim <- c(-4,4)
    
    critvals <- c(qt(0.05, degfree),qt(0.95,degfree))
    rect1 <- data.frame(xmin = min(critvals[1],xlim),xmax = critvals[1], ymin=-Inf,ymax=Inf)
    rect2 <- data.frame(xmin = critvals[2],xmax = max(critvals[2],xlim), ymin=-Inf,ymax=Inf)
    
   p <- switch(alternative,
    "two.sided" = p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE) + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE),
    "greater" = p + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE),
    "less" =  p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey20", alpha=0.5, inherit.aes = FALSE)
   )   
    p <- p + geom_vline(xintercept = tstat,lty=2,col="red") + xlim(xlim) + ggtitle(paste("T-distribution with ", degfree, "degrees of freedom"))
    print(p)
  })
  
  
  
}
)