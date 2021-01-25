## "The New York Times. (2021). Coronavirus (Covid-19) Data in the United States. 
## Retrieved 2020-01-23, 
## from https://github.com/nytimes/covid-19-data.


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
load("tssmrNYTimes.RData")

# Make sure there is row names correspondence 
tssmrNYTimes$TSMat <- SMRImposeRowIDs( smat = tssmrNYTimes$TSMat, rowIDs = rownames(tssmrNYTimes$SMR$M) )

# Search vectors
tsSearchVectors <- SparseMatrixRecommender::MakeTimeSeriesSearchVectors( tsMat = tssmrNYTimes$TSMat )
length(tsSearchVectors)

# Use dates
tssmrNYTimes$TIBNameToTIBRules <- setNames( as.POSIXct(names(tssmrNYTimes$TIBNameToTIBRules)), names(tssmrNYTimes$TIBNameToTIBRules) )

# Define UI for application that draws a histogram
ui <- SparseMatrixRecommenderInterfacesNoDT::TSCorrSMRMakeUI(tsSMR = tssmrNYTimes, tsSearchVectors = tsSearchVectors, 
                                                             initNNs = 8, initNCols = 2, plotOutputHeight = "800px",
                                                             dashboardTitle = "The New York Times COVID-19 Data Search Engine",
                                                             noteText = 
                                                               list(
                                                                 c(
                                                                   value = 
                                                                     paste(
                                                                       "The \"entities\" of the search engine are time series of COVID-19 infection cases and deaths over USA counties",
                                                                       "and the corresponding time series differences.",
                                                                       "The difference time series have the prefix \"Diff-\".")
                                                                 ),
                                                                 c(
                                                                   title = "[NYT1]",
                                                                   value =
                                                                     paste(
                                                                       "The New York Times, (2021), Coronavirus (Covid-19) Data in the United States, GitHub.",
                                                                       "Retrieved 2020-01-23 from https://github.com/nytimes/covid-19-data."),
                                                                   href = "https://github.com/nytimes/covid-19-data"
                                                                 ),
                                                                 c(
                                                                   title = "[AA1]",
                                                                   value = "Anton Antonov, \"NY Times COVID-19 data visualization (Update)\", (2021), MathematicaForPrediction at WordPress.",
                                                                   href = "https://mathematicaforprediction.wordpress.com/2021/01/15/ny-times-covid-19-data-visualization-update/"
                                                                 )
                                                               ),
                                                             theme = "flatly")

# Define server logic required to draw a histogram
server <- SparseMatrixRecommenderInterfacesNoDT::TSCorrSMRMakeServerFunction(tsSMR = tssmrNYTimes, tsSearchVectors = tsSearchVectors, roundDigits = 6)

# Run the application 
# shiny::runApp( TSCorrSMRCreateSearchInterface( tsSMR = tssmrNYTimes ) )
shinyApp( ui = ui, server = server)
