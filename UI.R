####shiny::runGitHub("OneSidedT","markdunning")

library(shiny)


shinyUI(navbarPage("Explore the two-sample t-test",id="nav",
                   
                   tabPanel("About",
                            sidebarLayout(
                              sidebarPanel(img(src="cruk-cambridge-institute.jpg",width=350,height=77), br(),a("cruk.cam.ac.uk",href="http://www.cruk.cam.ac.uk",target="_blank")),
                              mainPanel(helpText("This app was developed by the Bioinformatics Core of Cancer Research Uk Cambridge Institute to accompany a training course. On the course webpage you will find lecture notes from the course and practical exercises that use this app"),
                                        a("Introduction to Statistical Analysis",href="http://bioinformatics-core-shared-training.github.io/IntroductionToStats/"),
                                        br(),
                                        helpText(),
                                        br(),
                                        br(),
                                        
                                        br(),
                                        br(),
                                        a("View source Code for app", href="https://github.com/bioinformatics-core-shared-training/TwoSampleTest.git")
                              )
                            )
                            
                   ),
                   tabPanel("Data Input",
                            sidebarLayout(
                              sidebarPanel(h2("Data Import Parameters"),
                                           fileInput('file1', 'Choose CSV File',
                                                     accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                           helpText("If your file contains column headings, keep this box ticked"),
                                           checkboxInput('header', 'Header', TRUE),
                                           radioButtons('sep', 'Separator',
                                                        c(Comma=',',
                                                          Semicolon=';',
                                                          Tab='\t'),
                                                        selected=","),
                                           radioButtons('quote', 'Quote',
                                                        c(None='',
                                                          'Double Quote'='"',
                                                          'Single Quote'="'"),
                                                        selected='"'),
                                           textInput("skip", "Number of rows to skip in data file before reading data",value=0),
                                           checkboxInput('factors', '1st column is a factor?', FALSE),
                                           h2("Are your samples paired?"),
                                           helpText("If your two groups are dependent, you should choose a paired test. If your groups are independant, leave this box un-ticked"),
                                           checkboxInput('paired', 'Paired Samples?', FALSE),
                                           
                                           h2("Direction of comparison"),
                                           radioButtons("testDirection", "What comparison do you want to make?",c("A vs B", "B vs A")),
                                           helpText("If A vs B is selected, the difference will be the first column minus the second. Selecting B vs A will compute the second column minus the first"),
                                           
                                           helpText("You can choose to transform the data prior to statistical testing"),
                                           radioButtons("transform","Transformation",c("None"="none","Log10"="log.10","Log2"="log.2","Natural Log"="log"),"none")
                                           
                                           
                              )
                              ,
                              mainPanel(verbatimTextOutput("testDirection"),dataTableOutput("mytable")
                              )
                              
                            )
                            
                   ),
                   tabPanel("Data Distribution",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Display Parameters"),
                                checkboxInput("violin", "Overlay density on boxplot?", value=FALSE),
                                helpText("You can use the algorithm in R to guess how many bins to use in the histogram"),
                                checkboxInput("default.bins",label="Guess optimal bin size?",value=TRUE),
                                helpText("Otherwise, you can choose your own number of bins"),
                                sliderInput("bins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30),
#                                checkboxInput("showCI", "Show Confidence Interval",value = FALSE),
                                checkboxInput("sameScale","Use same scale on x-axis for both groups",value=FALSE)

                                
                              ),
                              mainPanel(helpText("The boxplot and histogram of the data are shown below"),
                                        plotOutput("boxplot"),
                                        h3("Basic Summary"),br(),
                                        verbatimTextOutput("summary"),
                                        plotOutput("histogram")
                                        )
                                        
                              )
                            ),
                   tabPanel("Differences",
                            
                            sidebarLayout(
                              sidebarPanel(
                                helpText("If you have selected a paired analysis, you will be able to assess the distribution of the differences here"),
                                h2("Display Parameters"),
                                checkboxInput("violin.paired", "Overlay density on boxplot?", value=FALSE),
                                helpText("You can use the algorithm in R to guess how many bins to use in the histogram"),
                                checkboxInput("default.bins.paired",label="Guess optimal bin size?",value=TRUE),
                                helpText("Otherwise, you can choose your own number of bins"),
                                sliderInput("bins.paired",
                                            "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30)
                              ),
                              mainPanel(
                                plotOutput("boxplot.paired"),
                                plotOutput("histogram.paired") 
                              )
                              
                              
                            )
                            
                            ),
                   tabPanel("Statistical Analysis",
                            sidebarLayout(
                              sidebarPanel(helpText("Use the histograms and boxplot to judge whether you need to use a parametric, or non-parametric test"),
                                               checkboxInput("do.parametric",label = "Use Parametric Test?",value = TRUE),
                                           helpText("If the differences are symmetrical about 0, then a wilcox signed-rank test will be used if a non-parametric test is specifed. Un-tick this box if the differences are not symmetrical. A sign test will be used instead"),
                                           checkboxInput("symmetrical","Symmetrical Differences",TRUE),
                                           h3("Variances"),
                                           helpText("Inspect the histograms and boxplots, or use the result of the F-test to judge whether the variances of each group are approximately the same"),
                                           checkboxInput('var.equal', 'Equal Variances?', FALSE),
                                           helpText("Note that changing this option will have no effect for a non-parametric test"),
                                           br(),
                                           helpText(""),
                                           radioButtons("alternative", "Alternative", c("Two-sided"="two.sided", "Greater" = "greater", "Lower"="less"),"two.sided")                                
                                           
                              ),
                              mainPanel(
                                        h4("Two-sample test"),
                                        verbatimTextOutput("ttest"),
                                        helpText("If you have chosen a Parametric test, the comparison of the calculated test-statistic to the reference distribution will be shown here"),
                                        plotOutput("tdist"),
                                        h4("F test for differences in variance"),
                                        helpText("F test to compare the variances of two samples from normal populations"),
                                        helpText("WARNING: Please use the result of this test with caution. Sometimes you can better judge differences in variance by inspecting the data distribution"),
                                        verbatimTextOutput("vartest")
                                        )
                              
                              
                            )
                   ),
                   tabPanel("Reproducible Analysis",
                            
                            sidebarLayout(
                              sidebarPanel(    h2("Report Parameters"),
                                               #submitButton ('Generate R Code', icon('toggle-right'))
                                               textInput("outfile", "What to call the output R script",value="analysis"),
                                               textInput("name", "Your Name",value="Anon."),
                                               textInput("title", "What title to use in the report",value="My R Analysis")),
                              mainPanel(
                                h4("R Script"),
                                
                                helpText("You will be able to re-run this analysis in R by downloading the R code below"),
                                helpText("We recommend RStudio to run the R code and compile a pdf or HTML report that will show the results of your analysis along with the code used"),
                                img(src="https://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"), br(),a("RStudio",href="https://www.rstudio.com/"),br(),
                                strong("The input file that you are analysing must be in your R working directory in order for the script to run"),
                                helpText("In order to compile the report in RStudio, you will need to install the ggplot2 package"),
                                code("install.packages('ggplot2'))"),
                                p(),
                                downloadLink('downloadScript', 'Download R Script'),
                                p(),
                                downloadLink('downloadMarkdown', 'Download R Markdown file')
                                
                              )
                              
                            )
                    )
                            
                   
              
                            
                            
                            
                    )
        
)
                   
