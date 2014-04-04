####shiny::runGitHub("TwoSampleT","markdunning")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Two-sample t-test Example"),
  
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    textInput("dataCol1", "Column containing data to test",value=1),
    textInput("dataCol2", "Column containing data to test",value=2),
    
    br(),    
    br(),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
    checkboxInput('paired', 'Paired Test', FALSE),
    
    h4("To do"),
    helpText("Try changing the mean height of females,
             Can you determine its effect on the Summary Statistics?")
    ),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("The data", dataTableOutput("mytable")),
      tabPanel("Boxplot",plotOutput("boxplot")),
      tabPanel("Histogram",plotOutput("histogram")),
      #tabPanel("t test", h4("Screen output in R"),
       #        plotOutput("zdist"),
        #       verbatimTextOutput("ttest")),
      
      tabPanel("t test",verbatimTextOutput("ttest"))
      tabPanel("Summary Statistics",
               h4("Screen output in R"),
               verbatimTextOutput("summary")),
      tabPanel("Example  R code",
               helpText("#Generate some data and place in a dataframe"),
               helpText("X <- 1:100"),
               helpText("Y <- 100 + 2*X + rnorm(100, sd=10)"),
               helpText("mydata <- data.frame(X, Y)"),
               br(),
               helpText("#Fit regression model"),
               helpText("model <- lm(Y ~ X, data=mydata)"),
               br(),
               helpText("#Summary statistics for the model"),
               helpText("summary(model)"),
               br(),
               helpText("#Plot the data and add the regression line"),
               helpText("plot(Y ~ X, data=mydata)"),
               helpText("abline(model)")
      )
    )
  )
  
  ))