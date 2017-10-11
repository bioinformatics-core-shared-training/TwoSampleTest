library(shiny)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tidyr)

shinyServer(function(input, output){
  
  data <- reactive({inFile <- input$file1
  
                    if (is.null(inFile))
                    return(data.frame(A = c(20.77,9.08,9.8,8.13,16.54,11.36,11.47,12.1,14.04,16.82,6.32,17.51,9.87,12.41,7.39,9.23,4.06,8.26,10.24,14.64),
                                      B=c(15.51,12.93,11.5,16.07,15.51,17.66,11.25,13.65,14.28,13.21,10.28,12.41,9.63,14.75,9.81,13.02,12.33,11.9,8.98,11.29)))
                    #print(inFile$datapath)
                    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
                    #read.csv("GraphPad Course Data/diseaseX.csv")
                    
                    if(input$factors) {
                    
                      colnames(data) <- c("variable","value")
                      data <- data[order(data[,1]),]
                      data$Index <- c(1:table(data[,1])[1], 1:table(data[,1])[2])
                      data <- spread(data,variable,value)
                      data <- data[,2:3]
                    } 
                    
                    data
  })

  output$testDirection = renderText({
    df <- data()
    if(input$testDirection == "A vs B") text <- paste("Comparison will be made in the direction", colnames(df)[1], "versus", colnames(df[2]))
    else text <- paste("Comparison will be made in the direction", colnames(df)[2], "versus", colnames(df[1]))
    text
  })
  output$mytable= renderDataTable({
    df <- data()

    if(!input$transform =="none"){
      
      df<- switch(input$transform,
                             log.2 = log2(df),
                             log.10 = log10(df),
                             log = log(df)
      )
    }
    
    
    if(input$paired){
      
      newDf <- df
      
      if(input$testDirection == "A vs B") {
        newDf$Difference=newDf[,1] - newDf[,2]
      } else newDf$Difference=newDf[,2] - newDf[,1]
      
      newDf$Sign <- "="
      newDf$Sign[newDf$Difference > 0] <- "+"
      newDf$Sign[newDf$Difference < 0] <- "-"
      df <- newDf
      }
    

    df
  }
  )
  

  output$histogram<- renderPlot({
    
    df <- data()
    
    if(!input$transform =="none"){
      
      df <- switch(input$transform,
                         log.2 = log2(df),
                         log.10 = log10(df),
                         log = log(df)
      )
    }
    
    df1 <- data.frame(value=df[,1],variable="X")
    df1 <- df1[!is.na(df[,1]),]
    df2 <- data.frame(value=df[,2],variable="X")
    df2 <- df2[!is.na(df2[,1]),]
    
    lims <- range(c(df[,1],df[,2]))
    
    if(input$default.bins){
      
      
      brx <- pretty(range(df1$value), 
                    n = nclass.Sturges(df1$value),min.n = 1)

      p1 <- ggplot(df1, aes(x=value)) + geom_histogram(breaks=brx,colour="black", fill=rgb(29,0,150,maxColorValue=255),alpha=0.5) + ylab("") 
      
      if(input$sameScale) p1 <- p1 + xlim(lims)
      
      
      brx <- pretty(range(df2$value), 
                    n = nclass.Sturges(df2$value),min.n = 1)
      
      p2 <- ggplot(df2, aes(x=value)) + geom_histogram(breaks=brx,colour="black", fill=rgb(236,0,140,maxColorValue=255),alpha=0.5) + ylab("") 
      
      if(input$sameScale) p2 <- p2 + xlim(lims)
      
      p <- grid.arrange(p1,p2,ncol=2)
      

    } else {
      x <- df1$value
      binwid <- (max(x)-min(x)) / input$bins
      print(binwid)
      
      p1 <- ggplot(df1, aes(x=value)) + geom_histogram(binwidth=binwid,colour="black", fill=rgb(29,0,150,maxColorValue=255)) + ylab("")
      
      if(input$sameScale) p1 <- p1 + xlim(lims)
      
      x <- df2$value
      binwid <- (max(x)-min(x)) / input$bins
      print(binwid)
      
      p2 <- ggplot(df2, aes(x=value)) + geom_histogram(binwidth=binwid,colour="black", fill=rgb(236,0,140,maxColorValue=255)) + ylab("")
      
      if(input$sameScale) p2 <- p2 + xlim(lims)
      
      p <- grid.arrange(p1,p2,ncol=2)

    }
    

    
    print(p)
  }
  
  )
  
  
  output$histogram.paired <- renderPlot({
    
  df <- data()
  
  if(!input$transform =="none"){
    
    df$value <- switch(input$transform,
                       log.2 = log2(df$value),
                       log.10 = log10(df$value),
                       log = log(df$value)
    )
  }
  

  df1 <- df[,1]
  df2 <- df[,2]

  if(input$paired){

    
    if (input$testDirection == "A vs B") {
      df <- data.frame(X=df[,1] - df[,2])
    } else df <- data.frame(X=df[,2] - df[,1])
    
    if(input$default.bins.paired){
      brx <- pretty(range(df$X), 
                    n = nclass.Sturges(df$X),min.n = 1)
      p <- ggplot(df, aes(x=X)) + geom_histogram(breaks=brx,colour="black", fill=rgb(29,0,150,maxColorValue=255)) + ylab("") 
    }
    
    else {
      binwid <- (max(df$X)-min(df$X)) / input$bins.paired
      print(binwid)
      p<- ggplot(df, aes(x=X)) + geom_histogram(binwidth=binwid,colour="black", fill=rgb(29,0,150,maxColorValue=255)) + ylab("")
    }
  } else p <- ggplot()

  print(p)
  }
  )
  
  

  
  output$boxplot<- renderPlot({
    
    df <- data()
    df <- melt(df)
    if(!input$transform =="none"){
      
      df$value <- switch(input$transform,
                         log.2 = log2(df$value),
                         log.10 = log10(df$value),
                         log = log(df$value)
      )
    }
    
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
   #mdf <- melt(df[,c(datacol1,datacol2)])
    
    if(input$violin){
      p <- ggplot(df, aes(x = variable,y=value,fill=variable)) + geom_violin(alpha=0.5) + geom_boxplot(fill="white",width=0.1) + geom_jitter(position = position_jitter(width = .05)) + coord_flip() + scale_fill_manual(values=c(rgb(29,0,150,maxColorValue=255), rgb(236,0,140,maxColorValue=255)))
    } else{
      p <- ggplot(df, aes(x = variable,y=value,fill=variable)) + geom_boxplot(alpha=0.5) + geom_jitter(position = position_jitter(width = .05)) + coord_flip() + scale_fill_manual(values=c(rgb(29,0,150,maxColorValue=255), rgb(236,0,140,maxColorValue=255)))
    }

#    if(input$showCI) p <- p + stat_summary(fun.data="mean_cl_normal",colour="red",fun.args = list(mult=1.96),geom="errorbarh")
    
    print(p)
    
  }
  )
  
  output$boxplot.paired<- renderPlot({
    
    df <- data()
    
    if(!input$transform =="none"){
      
      df$value <- switch(input$transform,
                         log.2 = log2(df$value),
                         log.10 = log10(df$value),
                         log = log(df$value)
      )
    }

    if(input$paired){
      
      if(input$testDirection == "A vs B") {
      df <- data.frame(value=df[,1] - df[,2],variable="Difference")
      } else       df <- data.frame(value=df[,2] - df[,1],variable="Difference")

    #mdf <- melt(df[,c(datacol1,datacol2)])
    
      if(input$violin.paired){
        p <- ggplot(df, aes(x = variable,y=value)) + geom_violin(fill=rgb(236,0,140,maxColorValue=255)) + geom_boxplot(fill="white",width=0.1) + geom_jitter(position = position_jitter(width = .05)) + coord_flip()
      } else{
        p <- ggplot(df, aes(x = variable,y=value)) + geom_boxplot(fill=rgb(236,0,140,maxColorValue=255)) + geom_jitter(position = position_jitter(width = .05)) + coord_flip() 
        }
      
    } else p <- ggplot()

      print(p)
      
      
  }
  )

  output$vartest <-renderPrint({
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
    if(!input$transform =="none"){
      
      df$value <- switch(input$transform,
                         log.2 = log2(df$value),
                         log.10 = log10(df$value),
                         log = log(df$value)
      )
    }
    
    
    var.test(df[,1],df[,2])
    }
  
  )
  
  
  
  output$ttest <-renderPrint({
    df <- data()
    #datacol1 <- as.numeric(input$dataCol1)
    #datacol2 <- as.numeric(input$dataCol2)
    
    if(!input$transform =="none"){
      
      df$value <- switch(input$transform,
                         log.2 = log2(df$value),
                         log.10 = log10(df$value),
                         log = log(df$value)
      )
    }
    
    #X <- df[,datacol1]
    #Y <- df[,datacol2]
    
    alternative = input$alternative
    paired <- as.logical(input$paired)
    var.equal <- as.logical(input$var.equal)    
    if(input$testDirection == "B vs A") {
      
      df2 <- data.frame(df[,2],df[,1])
      df <- df2
    
    }
    if(input$do.parametric) t.test(df[,1],df[,2],alternative=alternative,paired=paired,var.equal=var.equal)
    else {
      
      if(paired){
      
        if (input$symmetrical) wilcox.test(df[,1],df[,2],alternative=alternative,paired=paired,var.equal=var.equal)
        else{
          if(input$testDirection == "B vs A") {
            
            df2 <- data.frame(df[,2],df[,1])
            df <- df2
            
          }
            
          npos <- sum(df[,1]>df[,2])
          nneg <- sum(df[,1] < df[,2])
          x <- min(npos,nneg)
          n <- sum(df[,1] != df[,2])

          cat(paste("Number of +'s", npos,"\n"))
          cat(paste("Number of -'s", nneg,"\n"))
          cat(paste("Test statistic:", x,"\n"))
          pv <- round(pbinom(q = x, size = n,prob = 0.5)*2,3)
          
          if(input$alternative != "two.sided") pv <- pv/2
          
          cat(paste("P-value using binomial distribution with",n, "trials and p=0.5:",pv,"\n"))
          
        }
      } 
      else wilcox.test(df[,1],df[,2],alternative=alternative,paired=paired,var.equal=var.equal)
    }
  })

  output$summary <- renderPrint({
    df <- data()
    if(!input$transform =="none"){
      
      df$value <- switch(input$transform,
                         log.2 = log2(df$value),
                         log.10 = log10(df$value),
                         log = log(df$value)
      )
    }
    df <- melt(df)
    
    sumry <- lapply(split(df$value,df$variable),RcmdrMisc::numSummary)
    se <- sumry[[1]][[2]][,"sd"] / sqrt(sumry[[1]]$n)
    ci.lower <- sumry[[1]][[2]][,"mean"] - 1.96 * se
    names(ci.lower) <- "CI.lower"
    
    ci.upper <- sumry[[1]][[2]][,"mean"] + 1.96 * se
    names(ci.upper) <- "CI.upper"
    sumry[[1]]$table <- cbind(sumry[[1]]$table, ci.lower,ci.upper)
    
    se <- sumry[[2]][[2]][,"sd"] / sqrt(sumry[[1]]$n)
    ci.lower <- sumry[[2]][[2]][,"mean"] - 1.96 * se
    names(ci.lower) <- "CI.lower"
    
    ci.upper <- sumry[[2]][[2]][,"mean"] + 1.96 * se
    names(ci.upper) <- "CI.upper"
    sumry[[2]]$table <- cbind(sumry[[2]]$table, ci.lower,ci.upper)
    
    sumry

  })
  
  output$adv.summary <- renderPrint({
    df <- data()
    by(df$value,df$variable,stat.desc,basic=FALSE,norm=TRUE)
  })
  
  output$tdist <- renderPlot({
  
    if(input$do.parametric){
    
      df <- data()
      if(!input$transform =="none"){
        
        df$value <- switch(input$transform,
                           log.2 = log2(df$value),
                           log.10 = log10(df$value),
                           log = log(df$value)
        )
      }
      #datacol1 <- as.numeric(input$dataCol1)
      #datacol2 <- as.numeric(input$dataCol2)
      
      #X <- df[,datacol1]
      #Y <- df[,datacol2]
      alternative = input$alternative
      paired <- as.logical(input$paired)
      var.equal <- as.logical(input$var.equal)    
      if(input$testDirection == "B vs A") {
        
        df2 <- data.frame(df[,2],df[,1])
        df <- df2
        
      }
      tt <- t.test(df[,1], df[,2],alternative=alternative,paired=paired,var.equal=var.equal)
  
      tstat <- tt$statistic
      degfree <- tt$parameter
  
      alternative = input$alternative
      
      df <- data.frame(ts = rt(10000,df=degfree))
  
      
      #p<- ggplot(df, aes(x=ts)) + 
       # geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
        #               binwidth=.5,
         #              colour="black", fill="white") +
        #geom_density()
      #
     p <- ggplot(data.frame(x=c(-4,4)),aes(x)) + stat_function(fun=dt, args=list(df=degfree))
     
     xlim <- c(-4,4)
      
     if (alternative == "two.sided") critvals <- c(qt(0.025, degfree),qt(0.975,degfree))
    else critvals <- c(qt(0.05, degfree),qt(0.95,degfree))
      
      rect1 <- data.frame(xmin = min(critvals[1],xlim),xmax = critvals[1], ymin=-Inf,ymax=Inf)
      rect2 <- data.frame(xmin = critvals[2],xmax = max(critvals[2],xlim), ymin=-Inf,ymax=Inf)
      
     p <- switch(alternative,
      "two.sided" = p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="yellow", alpha=0.5, inherit.aes = FALSE) + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="yellow", alpha=0.5, inherit.aes = FALSE),
      "greater" = p + geom_rect(data=rect2,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="yellow", alpha=0.5, inherit.aes = FALSE),
      "less" =  p + geom_rect(data=rect1,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),fill="yellow", alpha=0.5, inherit.aes = FALSE)
     )   
      p <- p + geom_vline(xintercept = tstat,lty=2,col="red") + xlim(xlim) + ggtitle(paste("T-distribution with ", round(degfree,2), "degrees of freedom"))
      print(p)
    
    }
    
    
  })

  
  
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
      
        cat(file=file,as.name(paste0('myfile <- \"' , inFile$name, '\"\n')))
        cat(file=file,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
        cat(file=file,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
        cat(file=file,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
        cat(file=file,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
        cat(file=file,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
        if(!input$factors) cat(file=file,as.name("data <- reshape2::melt(data)\n"),append=TRUE)
        cat(file=file,as.name("colnames(data) <- c('variable','value')\n"),append=TRUE)
        
        }
    
      
      cat(file=file,as.name("head(data)\n"),append=TRUE)
      cat(file=file,as.name("library(ggplot2)\n"),append=TRUE)
      cat(file=file,as.name("p <- ggplot(data, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()\n"),append=TRUE)
      
      
      if(input$paired){
        cat(file=file,as.name("df <- data.frame(data, Observation = rep(1:(nrow(data)/2),2))\n"),append=TRUE)
        
        cat(file=file,as.name("p2 <- ggplot(df, aes(x = variable,y=value,col=as.factor(Observation),label=Observation,group=as.factor(Observation))) + geom_line() + geom_text() + coord_flip()\n"),append=TRUE)
        cat(file=file,as.name("gridExtra::grid.arrange(p,p2)\n"),append=TRUE)
      } 
      
      cat(file=file,as.name("p\n"),append=TRUE)
      
      cat(file=file,as.name("lapply(split(data$value,data$variable),summary)\n"),append=TRUE)
      
      cat(file=file,as.name("p <- ggplot(data, aes(x=value)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + facet_wrap(~variable) + stat_function(fun=dnorm,color='red',args=list(mean=mean(data$value), sd=sd(data$value)))\n"),append=TRUE)
      cat(file=file,as.name("var.test(value~variable,data=data)\n"),append=TRUE)
      
      
      if(input$paired){
        cat(file=file,as.name("newDf <- do.call(cbind,split(data$value,data$variable))\n"),append=TRUE)
        cat(file=file,as.name("Diff <- data.frame(Difference=newDf[,1] - newDf[,2])\n"),append=TRUE)
        
        cat(file=file,as.name("p2 <- ggplot(Diff,aes(x=Difference)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + stat_function(fun=dnorm, color='red', args=list(mean=mean(Diff$Difference), sd=sd(Diff$Difference)))\n"),append=TRUE)
        cat(file=file,as.name("p <- gridExtra::grid.arrange(p,p2)\n"),append=TRUE)
      } 
      cat(file=file,as.name("p\n"),append=TRUE)
      cat(file=file,as.name(paste0('alternative <- \'', input$alternative,'\'\n')),append=TRUE)
      cat(file=file,as.name(paste0('paired <- ',as.logical(input$paired),'\n')),append=TRUE)
      cat(file=file,as.name(paste0('var.equal <-',as.logical(input$var.equal),'\n')),append=TRUE)
      
      if(input$do.parametric){
        cat(file=file,as.name("t.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      } else cat(file=file,as.name("wilcox.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      
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

      } else{
      
        cat(file=script,as.name(paste0('myfile <- \"' , inFile$name, '\"\n')))
        cat(file=script,as.name(paste0('sep <- \'', input$sep,'\'','\n')),append=TRUE)
        cat(file=script,as.name(paste0('quote <- \'', input$quote,'\'','\n')),append=TRUE)
        cat(file=script,as.name(paste('header <- ', input$header,'\n')),append=TRUE)
        cat(file=script,as.name(paste('skip <- ', input$skip,'\n')),append=TRUE)
        cat(file=script,as.name("data <- read.csv(myfile, header=header, sep=sep, quote=quote,skip=skip)\n"),append=TRUE)
        
        
        if(!input$factors) cat(file=script,as.name("data <- reshape2::melt(data)\n"),append=TRUE)
        cat(file=script,as.name("colnames(data) <- c('variable','value')\n"),append=TRUE)
      }
      cat(file=script,as.name("head(data)\n"),append=TRUE)
      cat(file=script,as.name("library(ggplot2)\n"),append=TRUE)
      cat(file=script,as.name("p <- ggplot(data, aes(x = variable,y=value,fill=variable)) + geom_boxplot() + coord_flip()\n"),append=TRUE)
      
      if(input$paired){
        cat(file=script,as.name("df <- data.frame(data, Observation = rep(1:(nrow(data)/2),2))\n"),append=TRUE)
        
        cat(file=script,as.name("p2 <- ggplot(df, aes(x = variable,y=value,col=as.factor(Observation),label=Observation,group=as.factor(Observation))) + geom_line() + geom_text() + coord_flip()\n"),append=TRUE)
        cat(file=script,as.name("gridExtra::grid.arrange(p,p2)\n"),append=TRUE)
      } 
      cat(file=script,as.name("p\n"),append=TRUE)
      
      cat(file=script,as.name("lapply(split(data$value,data$variable),summary)\n"),append=TRUE)
      
      cat(file=script,as.name("p <- ggplot(data, aes(x=value)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + facet_wrap(~variable) + stat_function(fun=dnorm,color='red',args=list(mean=mean(data$value), sd=sd(data$value)))\n"),append=TRUE)
      
      
      if(input$paired){
        cat(file=script,as.name("newDf <- do.call(cbind,split(data$value,data$variable))\n"),append=TRUE)
        cat(file=script,as.name("Diff <- data.frame(Difference=newDf[,1] - newDf[,2])\n"),append=TRUE)
        
        cat(file=script,as.name("p2 <- ggplot(Diff,aes(x=Difference)) + geom_histogram(aes(y=..density..),colour='black',fill='white') + stat_function(fun=dnorm,color='red',args=list(mean=mean(Diff$Difference), sd=sd(Diff$Difference)))\n"),append=TRUE)
        cat(file=script,as.name("p <- gridExtra::grid.arrange(p,p2)\n"),append=TRUE)
      } 
      cat(file=script,as.name("var.test(value~variable,data=data)\n"),append=TRUE)
      
      cat(file=script,as.name("p\n"),append=TRUE)
      
      cat(file=script,as.name(paste0('alternative <- \'', input$alternative,'\'\n')),append=TRUE)
      cat(file=script,as.name(paste0('paired <-',as.logical(input$paired),'\n')),append=TRUE)
      cat(file=script,as.name(paste0('var.equal <-',as.logical(input$var.equal),'\n')),append=TRUE)
      
      if(input$do.parametric){
        cat(file=script,as.name("t.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      } else cat(file=script,as.name("wilcox.test(value~variable,data=data,alternative=alternative,paired=paired,var.equal=var.equal)\n"),append=TRUE)
      
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
