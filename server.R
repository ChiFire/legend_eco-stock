library(shiny)
library(quantmod)

source('function.R')

shinyServer(
     function(input,output){
          output$period = renderText({paste('The training period is from',
                                     input$start_date, 'to',
                                     input$end_date)})
          output$index = renderText({paste('The index to predit is',
                                            input$stock)})
          yahooData = eventReactive(input$run, {
               getSymbols(input$stock, src = 'yahoo', from = input$start_date,
                          to = input$end_date, auto.assign = FALSE)
          })
          
          output$yahooPlot = renderPlot({
               chartSeries(yahooData(),theme = 'white', type = 'line',
                           TA = 'addBBands();addCCI();addADX();addRSI();
                           addMACD()')
          })
          
          output$modelPlot = renderPlot({
               getModel(input$stock)
          })
          
          output$dataTable = renderTable({
               inFile = input$dataFile
               if(is.null(inFile)){
                    return(NULL)
               }
               head(read.csv(inFile$datapath)[,1:24],25)
          })
          
     }
)