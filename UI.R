####shiny::runGitHub("TwoSampleT","markdunning")
##runApp(".")

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Two-sample t-test Example"),
  
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
    h2("Analysis Parameters"),
    br(),    
    checkboxInput('factors', '1st column is a factor?', FALSE),
    
    br(),
    radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided"),
    checkboxInput('paired', 'Paired Test', FALSE),
    checkboxInput('var.equal', 'Equal Variances?', FALSE),
    textInput("outfile", "What to call the output R script",value="analysis"),
  textInput("name", "Your Name",value="Anon."),
  textInput("title", "What title to use in the report",value="My R Analysis")
),
  
  mainPanel(
    tabsetPanel(
#      tabPanel("Plot",plotOutput("plot")),
      tabPanel("The data", dataTableOutput("mytable"), helpText("If you have selected a paired analysis, a table of differences will appear below"),dataTableOutput("tableOfDiffs")),
      tabPanel("Data Distribution",plotOutput("boxplot"),
               verbatimTextOutput("summary")
               ),
      tabPanel("Histogram",plotOutput("histogram")),
      #tabPanel("t test", h4("Screen output in R"),
       #        plotOutput("zdist"),
        #       verbatimTextOutput("ttest")),
      
      tabPanel("t test",h4("Screen output in R"),plotOutput("tdist"),verbatimTextOutput("ttest")),

      tabPanel("R code",
               helpText("You will be able to re-run this analysis in R by downloading the R code below"),
               strong("The input file that you are analysing must be in your R working directory in order for the script to run"),
               h4("Code Preview"),
               verbatimTextOutput("thecode"),
               downloadLink('downloadScript', 'Download R Script'),
               br(),
               downloadLink('downloadMarkdown', 'Download R Markdown'),
               br(),
               #             downloadLink('downloadPDF', 'Download HTML Report')
               helpText("We recommend RStudio to run the R code and compile reports"),
               img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
               helpText("In order to compile the report in RStudio, you will need to install the ggplot2, rmarkdown and knitr packages"),br(),
               code("install.packages(c('knitr','ggplot2','rmarkdown'))")
      )
      )
    )
  
  
  )

)