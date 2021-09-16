library(shiny)
library(ggplot2)
library(plotly)
library(xts)
library(quantmod)
library(dygraphs)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(zoo)
library(recommenderlab)
library(ggplot2)                       
library(data.table)
library(reshape2)
library(tidyr)
library(dtplyr) # dplyr is part of dtplyr
library(dplyr)
library(R6)
library(PBSmodelling)
library(rmarkdown)
library(shinybusy)


shinyUI(fluidPage(

    # Application title
    titlePanel("Movie Recommendation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file_1","Please Upload File for Movie Database"),
            fileInput("file_2","Please Upload File for Rating Database"),
            uiOutput("select_user_id")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
            column(10,align = "center", withSpinner(tableOutput("print_movie")))
            
        )
    )
))
