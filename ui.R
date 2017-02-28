library(shiny)
library(shinydashboard)

shinyUI(fluidPage(
     
     titlePanel(h2('Decision Making System For U.S. Stock Indices Prediction',
                align = 'center')),
     sidebarLayout(position = 'right',
          sidebarPanel(helpText('Select an Index and period to analyze. 
                                Information will be collected from Yahoo Finance.'),
                       textInput('start_date','Choose start date',Sys.Date()-730),
                       textInput('end_date','Choose end date',Sys.Date()),
                       radioButtons('stock','Select the ticker',
                                    list('^DJI','^NYA','^IXIC','^GSPC'),
                                    selected = '^DJI'),
                       fileInput('dataFile','Load the data file',
                                 accept = c(
                                      'text/csv',
                                      'text/comma-separated-values,text/plain',
                                      '.csv')),
                       selectInput('method', 'Select the model to analysis',
                                   choices = c('QRF','QRNN')),
                       actionButton('run','Update')
                       ),
          mainPanel(
               tabsetPanel(type = 'tab',
                    tabPanel('Introduction',
                             includeMarkdown('abstract.md')),
                    tabPanel('Technical Analysis',
                             h3(helpText('Technical Analysis Based On Data From Yahoo Finance')),
                             plotOutput("yahooPlot")),
                    tabPanel('Macroeco Indicators',
                             fluidRow(
                                  column(width = 12,
                                         box(
                                              title = 'Sample (fist 25 lines)',width = NULL, status = 'primary',
                                              div(style ='overflow-x: auto',
                                                  tableOutput('dataTable'))
                                         ))
                                  )
                             ),
                    tabPanel('Decision Support System',
                             h3(helpText('Model is trained using data from 1992 to 2016')),
                             plotOutput("modelPlot")
                             )
               )
          )
     )
 
 
)  )