####shiny::runGitHub("TwoSampleT","markdunning")
##runApp(".")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Two-sample tests"),
  
  sidebarPanel(
    h2("Data Import Parameters"),
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
    textInput("skip", "Number of rows to skip in data file before reading data",value=0),
    checkboxInput('factors', '1st column is a factor?', FALSE),
    h2("Analysis Parameters"),
    br(),    
    checkboxInput("default.bins",label="Use Default Histogram Bin Width",value=TRUE),
    sliderInput("bins",
                "Number of bins:",
                min = 1,
                max = 50,
                value = 30),
    helpText("Use the histograms and boxplot to judge whether you need to use a parametric, or non-parametric test"),
    checkboxInput("do.parametric",label = "Use Parametric Test?",value = TRUE),
    helpText("Alternatively, you could choose to transform the data prior to statistical testing..."),
    radioButtons("transform","Transformation",c("None"="none","Log10"="log.10","Log2"="log.2","Natural Log"="log"),"none"),
    h3("Relationship between groups"),
    helpText("If your two groups are dependent, you should choose a paired test. If your groups are independant, leave this box un-ticked"),
    checkboxInput('paired', 'Paired Test', FALSE),
    helpText("If the differences are symmetrical about 0, then a wilcox signed-rank test will be used if a non-parametric test is specifed. Un-tick this box if the differences are not symmetrical. A sign test will be used instead"),
    checkboxInput("symmetrical","Symmetrical Differences",TRUE),
    h3("Variances"),
    helpText("Inspect the histograms and boxplots, or use the result of the F-test to judge whether the variances of each group are approximately the same"),
    checkboxInput('var.equal', 'Equal Variances?', FALSE),
    br(),
    helpText(""),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
  
    br(),
    h2("Report Parameters"),
    textInput("outfile", "What to call the output R script",value="analysis"),
  textInput("name", "Your Name",value="Anon."),
  textInput("title", "What title to use in the report",value="My R Analysis")
),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("The data", dataTableOutput("mytable")),
      tabPanel("Data Distribution",plotOutput("boxplot"),h3("Basic Summary"),br(),
               verbatimTextOutput("summary"),plotOutput("plotMeans")#,h3("Advanced Summary"),br(),verbatimTextOutput("adv.summary")
               ),
      tabPanel("Histogram",plotOutput("histogram"),helpText("F test to compare the variances of two samples from normal populations"),verbatimTextOutput("vartest")),
      #tabPanel("t test", h4("Screen output in R"),
       #        plotOutput("zdist"),
        #       verbatimTextOutput("ttest")),
      
      tabPanel("Test Result",h4("Screen output in R"),verbatimTextOutput("ttest"),helpText("If you have chosen a Parametric test, the comparison of the calculated test-statistic to the reference distribution will be shown here"),plotOutput("tdist")),

      tabPanel("Reproducible Analysis",
               h4("R Script"),
               
               helpText("You will be able to re-run this analysis in R by downloading the R code below"),
               helpText("We recommend RStudio to run the R code and compile a pdf or HTML report that will show the results of your analysis along with the code used"),
               img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
               strong("The input file that you are analysing must be in your R working directory in order for the script to run"),
               helpText("In order to compile the report in RStudio, you will need to install the ggplot2, rmarkdown, reshape2,gridExtra and knitr packages"),br(),
               code("install.packages(c('knitr','ggplot2','rmarkdown,'reshape2','gridExtra'))"),
               br(),
               downloadLink('downloadScript', 'Download R Script'),
               br(),
               br(),
               downloadLink('downloadMarkdown', 'Download R Markdown file')
      )
      )
    )
  
  
  )

)