shinyUI <- navbarPage(
  title = "Web Applet",
  
  tabPanel("Monthly Data",
           tabsetPanel(
             tabPanel("Adj. Close",
                      br(),
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h3("Adjusted Close"),
                            br(),
                            h4("Stock da visualizzare:"),
                            checkboxInput("checkboxAAPL", label = "Apple", value = TRUE),
                            checkboxInput("checkboxMSFT", label = "Microsoft", value = TRUE),
                            checkboxInput("checkboxAMZN", label = "Amazon", value = TRUE),
                            checkboxInput("checkboxGOOG", label = "Google", value = TRUE)
                          ),
                          mainPanel(dygraphOutput("plotAdjClose"))
                        )
                      )),
             tabPanel("CC Return",
                      br(),
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h3("CC Return"),
                            br(),
                            h4("Stock da visualizzare:"),
                            checkboxInput("checkboxAAPL_CC", label = "Apple", value = TRUE),
                            checkboxInput("checkboxMSFT_CC", label = "Microsoft", value = TRUE),
                            checkboxInput("checkboxAMZN_CC", label = "Amazon", value = TRUE),
                            checkboxInput("checkboxGOOG_CC", label = "Google", value = TRUE)
                          ),
                          mainPanel(dygraphOutput("plotCCReturn"))
                        )
                      ))
           )),
  
  tabPanel("Smt. Histogram",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "menuSmtHst",
                   "Stock:",
                   choices = c("Apple", "Microsoft", "Amazon", "Google")
                 ),
                 sliderInput(
                   "numbins",
                   "Numero di intervalli:",
                   min = 3,
                   max = 30,
                   value = 3
                 )
               ),
               mainPanel(plotOutput("plotSmoothedHistogram"))
             )
           )),
  
  tabPanel("Correlation",
           fluidPage(
             sidebarLayout(sidebarPanel(
               selectInput(
                 "menuCor",
                 "Coppie di Stock:",
                 choices = c(
                   "AAPL-MSFT",
                   "AAPL-AMZN",
                   "AAPL-GOOG",
                   "MSFT-AMZN",
                   "MSFT-GOOG",
                   "AMZN-GOOG"
                 )
               ),
             ),
             mainPanel(plotOutput("plotCor")))
           )),
  
  tabPanel("Beta",
           fluidPage(
             sidebarLayout(sidebarPanel(
               selectInput(
                 "menuBeta",
                 "Stock:",
                 choices = c("Apple", "Microsoft", "Amazon", "Google")
               ),
             ),
             mainPanel(
               h3("Beta coefficient computed on index SP500"),
               plotOutput("plotBeta")
               )
             )
           )),
  
  tabPanel("Portfolio",
           tabsetPanel(
             tabPanel("Value over time",
                      br(),
                      fluidPage(dygraphOutput("plotPFValue"))),
             tabPanel("SP500 Comparison",
                      br(),
                      fluidPage(dygraphOutput("plotPFComparison")))
           )),
  
  tabPanel("Summary",
           fluidPage(
             sidebarLayout(sidebarPanel(
               selectInput(
                 "menuSummary",
                 "Stock:",
                 choices = c("Apple", "Microsoft", "Amazon", "Google")
               ),
             ),
             mainPanel(plotOutput("plotSummary")))
           )),
  
  tabPanel("About",
           includeMarkdown("src/about.md"))
)
