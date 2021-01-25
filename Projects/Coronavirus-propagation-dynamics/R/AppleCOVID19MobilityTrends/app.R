## Appple mobility trends time series search engine
## https://covid19.apple.com/mobility

library(shiny)
library(Matrix)
library(tidyverse)
library(ggplot2)

library(purrr)
library(dplyr)
library(RcppRoll)
library(stringi)
library(stringr)

library(SMRMon)
library(OutlierIdentifiers)
library(SparseMatrixRecommender)
library(SparseMatrixRecommenderInterfacesNoDT)

# Get the TS-SMR object:
load("tssmrApple.RData")

# Make sure there is row names correspondence 
tssmrApple$TSMat <- SMRImposeRowIDs( smat = tssmrApple$TSMat, rowIDs = rownames(tssmrApple$SMR$M) )

# Search vectors
tsSearchVectors <- SparseMatrixRecommender::MakeTimeSeriesSearchVectors( tsMat = tssmrApple$TSMat )
length(tsSearchVectors)

# Use dates
tssmrApple$TIBNameToTIBRules <- setNames( as.POSIXct(names(tssmrApple$TIBNameToTIBRules)), names(tssmrApple$TIBNameToTIBRules) )

# Define UI for application that draws a histogram
ui <- SparseMatrixRecommenderInterfacesNoDT::TSCorrSMRMakeUI(tsSMR = tssmrApple, tsSearchVectors = tsSearchVectors, 
                                                             initNNs = 8, initNCols = 2, plotOutputHeight = "800px",
                                                             dashboardTitle = "Apple Mobility Trends Reports Search Engine",
                                                             noteText = 
                                                               list(
                                                                 c(
                                                                   value = 
                                                                     paste(
                                                                       "Most of the \"entities\" of the search engine are time series that reflect requests for directions in Apple Maps.",
                                                                       "Tempearature times series \"entities\" are also included: they correspond to temperature data at locations that are found in Apple's data.",
                                                                       "The temperature time series have the prefix \"MeanTemperature-\"."
                                                                     )
                                                                 ),
                                                                 c( title = "[AAPL1]", 
                                                                    value = "Apple mobility trends COVID-19 data from \"Mobility Trends Reports\".",
                                                                    href = "https://covid19.apple.com/mobility"),
                                                                 c( 
                                                                   title = "[AA1]",
                                                                   value = "Anton Antonov, \"Apple mobility trends data visualization (for COVID-19)\", (2020), MathematicaForPrediction at WordPress.", 
                                                                   href = "https://mathematicaforprediction.wordpress.com/2020/04/17/apple-mobility-trends-data-visualization-for-covid-19"
                                                                 )
                                                               ),
                                                             theme = "flatly")

# Define server logic required to draw a histogram
server <- SparseMatrixRecommenderInterfacesNoDT::TSCorrSMRMakeServerFunction(tsSMR = tssmrApple, tsSearchVectors = tsSearchVectors, roundDigits = 6)

# Run the application 
# shiny::runApp( TSCorrSMRCreateSearchInterface( tsSMR = tssmrApple ) )
shinyApp( ui = ui, server = server)
