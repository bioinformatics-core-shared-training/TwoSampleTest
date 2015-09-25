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
    textInput("skip", "Number of rows to skip in data file before reading data",value=0),
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
      tabPanel("Data Distribution",plotOutput("boxplot"),h3("Basic Summary"),br(),
               verbatimTextOutput("summary"),h3("Advanced Summary"),br(),verbatimTextOutput("adv.summary")
               ),
      tabPanel("Histogram",plotOutput("histogram")),
      #tabPanel("t test", h4("Screen output in R"),
       #        plotOutput("zdist"),
        #       verbatimTextOutput("ttest")),
      
      tabPanel("t test",h4("Screen output in R"),plotOutput("tdist"),verbatimTextOutput("ttest")),

      tabPanel("Reproducible Analysis",
               h4("R Script"),
               
               helpText("You will be able to re-run this analysis in R by downloading the R code below"),
               strong("The input file that you are analysing must be in your R working directory in order for the script to run"),
               helpText("In order to compile the report in RStudio, you will need to install the ggplot2, rmarkdown, reshape2,gridExtra and knitr packages"),br(),
               code("install.packages(c('knitr','ggplot2','rmarkdown,'reshape2','gridExtra'))"),
               br(),

               downloadLink('downloadScript', 'Download R Script'),
               br(),

               br(),
               #             downloadLink('downloadPDF', 'Download HTML Report')
               h4("Template to generate a reproducible document"),
               helpText("We recommend RStudio to run the R code and compile a pdf or HTML report that will show the results of your analysis along with the code used"),
               img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
               downloadLink('downloadMarkdown', 'Download R Markdown file')
      )
      )
    )
  
  
  )

)