library(shiny)
library(ggplot2)
library(reshape2)
library(gridExtra)


shinyServer(function(input, output){
  
  data <- reactive({inFile <- input$file1
  
                    if (is.null(inFile))
                    return(structure(list(variable = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
                                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
                                                                 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                 2L, 2L), .Label = c("Breed.A", "Breed.B"), class = "factor"), 
                                          value = c(20.77, 9.08, 9.8, 8.13, 16.54, 11.36, 11.47, 12.1, 
                                                    14.04, 16.82, 6.32, 17.51, 9.87, 12.41, 7.39, 9.23, 4.06, 
                                                    8.26, 10.24, 14.64, 15.51, 12.93, 11.5, 16.07, 15.51, 17.66, 
                                                    11.25, 13.65, 14.28, 13.21, 10.28, 12.41, 9.63, 14.75, 9.81, 
                                                    13.02, 12.33, 11.9, 8.98, 11.29)), row.names = c(NA, -40L
                                                    ), .Names = c("variable", "value"), class = "data.frame"))
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
  
  output$tableOfDiffs= renderDataTable({
    if(input$paired){
      df <- data()
      newDf <- do.call(cbind,split(df$value,df$variable))
      Diff <- data.frame(Observation = 1:nrow(newDf),newDf,Difference=newDf[,1] - newDf[,2])
      Diff
    }
  }
  )

  
  output$histogram<- reactivePlot(function(){
    
  df <- data()
  

  
  p<- ggplot(df, aes(x=value)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis

                   colour="black",fill="white") + facet_wrap(~variable)
  
  p <- p + stat_function(fun=dnorm,
                         color="red",
                         arg=list(mean=mean(df$value), 
                                  sd=sd(df$value)))
  
  if(input$paired){
    newDf <- do.call(cbind,split(df$value,df$variable))
    Diff <- data.frame(Difference=newDf[,1] - newDf[,2])
    
    p2 <- ggplot(Diff,aes(x=Difference)) + geom_histogram(aes(y=..density..),colour="black",fill="white") + stat_function(fun=dnorm,
                                                                                                                          color="red",
                                                                                                                          arg=list(mean=mean(Diff$Difference), 
                                                                                                                                   sd=sd(Diff$Difference)))
    gridExtra::grid.arrange(p,p2)
  } else p
  }
  )
  
  output$boxplot<- reactivePlot(function(){
    
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
   #mdf <- melt(df[,c(datacol1,datacol2)])
    
    p <- ggplot(df, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()
    
    if(input$paired){
      df <- data.frame(df, Observation = rep(1:(nrow(df)/2),2))
      
      p2 <- ggplot(df, aes(x = variable,y=value,col=as.factor(Observation),label=Observation,group=as.factor(Observation))) + geom_line() + geom_text() + coord_flip()
      gridExtra::grid.arrange(p,p2)
    } else p
    


    
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
  output$thecode <- renderPrint({
    
    inFile <- input$file1
    
    if(is.null(inFile$name)){
      
      print(as.name("data <- data.frame(variable=c(rep('Breed.A',20),rep('Breed.B',20)),value=c(20.77,9.08,9.8,8.13,16.54,11.36,11.47,12.1,14.04,16.82,6.32,17.51,9.87,12.41,
                                                                                               7.39,9.23,4.06,8.26,10.24,14.64,15.51,12.93,11.5,16.07,15.51,17.66,11.25,13.65,
                                                                                               14.28,13.21,10.28,12.41,9.63,14.75,9.81,13.02,12.33,11.9,8.98,11.29))\n"))
      
    } else{

      print(as.name(paste0('myfile <- ' , inFile$name)))
      
      print(as.name(paste0('sep <- \'', input$sep,'\'')))
      print(as.name(paste0('quote <- \'', input$quote,'\'')))
      print(as.name(paste('header <- ', input$header)))
      print(as.name(paste('skip <- ', input$skip)))
      print(as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)"))
      
      if(!input$factors) print(as.name("data <- melt(data)"))
      print(as.name("colnames(data) <- c('variable','value')"))
    }
    
    print(as.name("head(data)"))
    print(as.name("library(ggplot2)"))
    print(as.name("ggplot(data, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip())"))
    
    print(as.name("lapply(split(data$value,data$variable),summary)"))
    
    print(as.name("ggplot(data, aes(x=value)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + facet_wrap(~variable) + stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$value), sd=sd(data$value)))"))
    
    print(as.name(paste0('alternative <- \'', input$alternative,'\'')))
    print(as.name(paste0('paired <- \'',as.logical(input$paired),'\'')))
    print(as.name(paste0('var.equal <- \'',as.logical(input$var.equal),'\'')))
    
    
    print(as.name("t.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)"))

    print(as.name("sessionInfo()"))
  }
  )
  
  
  output$downloadScript <- downloadHandler(
    filename = function() {
      paste(input$outfile, '.R', sep='')
    },
    content = function(file) {
      inFile <- input$file1
      message(inFile)
      if(is.null(inFile)){
        
        cat(file=file,as.name("data <- data.frame(variable=c(rep('Breed.A',20),rep('Breed.B',20)),value=c(20.77,9.08,9.8,8.13,16.54,11.36,11.47,12.1,14.04,16.82,6.32,17.51,9.87,12.41,
                                                                                               7.39,9.23,4.06,8.26,10.24,14.64,15.51,12.93,11.5,16.07,15.51,17.66,11.25,13.65,
                                                                                               14.28,13.21,10.28,12.41,9.63,14.75,9.81,13.02,12.33,11.9,8.98,11.29))\n"))
        message("Writing to file.....")
      } else {
      
        cat(file=file,as.name(paste0('myfile <- ' ,inFile$name, '\n')))
        cat(file=file,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
        cat(file=file,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
        cat(file=file,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
        cat(file=file,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
        cat(file=file,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
        if(!input$factors) cat(file=file,as.name("data <- melt(data)\n"),append=TRUE)
        cat(file=file,as.name("colnames(data) <- c('variable','value')\n"),append=TRUE)
        
        }
    
      
      cat(file=file,as.name("head(data)\n"),append=TRUE)
      cat(file=file,as.name("library(ggplot2)\n"),append=TRUE)
      cat(file=file,as.name("ggplot(data, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()\n"),append=TRUE)
      
      cat(file=file,as.name("lapply(split(data$value,data$variable),summary)\n"),append=TRUE)
      
      cat(file=file,as.name("ggplot(data, aes(x=value)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + facet_wrap(~variable) + stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$value), sd=sd(data$value)))\n"),append=TRUE)
      
      cat(file=file,as.name(paste0('alternative <- \'', input$alternative,'\'\n')),append=TRUE)
      cat(file=file,as.name(paste0('paired <- ',as.logical(input$paired),'\n')),append=TRUE)
      cat(file=file,as.name(paste0('var.equal <-',as.logical(input$var.equal),'\n')),append=TRUE)
      
      
      cat(file=file,as.name("t.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      
      #formatR::tidy_source(source=file,output = file)
    }
  )
  
  
  output$downloadMarkdown <- downloadHandler(
    filename = function() {
      paste(input$outfile, '.Rmd', sep='')
    },
    content = function(file) {
      inFile <- input$file1
      script <- gsub(".Rmd", ".R",file)
      if(is.null(inFile)){
        
        cat(file=script,as.name("data <- data.frame(variable=c(rep('Breed.A',20),rep('Breed.B',20)),value=c(20.77,9.08,9.8,8.13,16.54,11.36,11.47,12.1,14.04,16.82,6.32,17.51,9.87,12.41,
                                                                                               7.39,9.23,4.06,8.26,10.24,14.64,15.51,12.93,11.5,16.07,15.51,17.66,11.25,13.65,
                                                                                               14.28,13.21,10.28,12.41,9.63,14.75,9.81,13.02,12.33,11.9,8.98,11.29))\n"))
        message("Writing to file.....")
      } else{
      
        cat(file=script,as.name(paste0('myfile <- ' ,inFile$name, '\n')))
        cat(file=script,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
        cat(file=script,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
        cat(file=script,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
        cat(file=script,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
        cat(file=script,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
        
        
        if(!input$factors) cat(file=file,as.name("data <- melt(data)\n"),append=TRUE)
        cat(file=script,as.name("colnames(data) <- c('variable','value')\n"),append=TRUE)
      }
      cat(file=script,as.name("head(data)\n"),append=TRUE)
      cat(file=script,as.name("library(ggplot2)\n"),append=TRUE)
      cat(file=script,as.name("ggplot(data, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()\n"),append=TRUE)
      
      cat(file=script,as.name("lapply(split(data$value,data$variable),summary)\n"),append=TRUE)
      
      cat(file=script,as.name("ggplot(data, aes(x=value)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + facet_wrap(~variable) + stat_function(fun=dnorm,color='red',arg=list(mean=mean(data$value), sd=sd(data$value)))\n"),append=TRUE)
      
      cat(file=script,as.name(paste0('alternative <- \'', input$alternative,'\'\n')),append=TRUE)
      cat(file=script,as.name(paste0('paired <-',as.logical(input$paired),'\n')),append=TRUE)
      cat(file=script,as.name(paste0('var.equal <-',as.logical(input$var.equal),'\n')),append=TRUE)
      
      
      cat(file=script,as.name("t.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      knitr:::spin(hair=script,knit = FALSE)
      rmd <- readLines(file)
      
      cat(file = file, paste(input$title, "\n=======================\n"))
      cat(file=file, as.name(paste("###", input$name, "\n")),append=TRUE)    
      cat(file=file, as.name(paste("### Report Generated at: ", as.character(Sys.time()), "\n")),append=TRUE)    
      
      for(i in 1:length(rmd)){
        cat(file=file, as.name(paste(rmd[i], "\n")),append=TRUE)
        
      }
      
      #    formatR::tidy_urce(file,output = file)
    }
  )
  
}
)