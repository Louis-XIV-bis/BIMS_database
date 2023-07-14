
#####################################################################
# Initialisation des packages 

options(timeout = 2000)
if (!require('tibble', quietly = T)) install.packages('tibble');
if (!require('shiny', quietly = T)) install.packages('shiny');
if (!require('shinydashboard', quietly = T)) install.packages('shinydashboard');
if (!require('shinybusy', quietly = T)) install.packages('shinybusy');
if (!require('shinycssloaders', quietly = T)) install.packages('shinycssloaders');
if (!require('shinyjs', quietly = T)) install.packages('shinyjs');
if (!require('shinyalert', quietly = T)) install.packages('shinyalert');
if (!require('stringr', quietly = T)) install.packages('stringr');
if (!require('stringi', quietly = T)) install.packages('stringi');
if (!require('DT', quietly = T)) install.packages('DT');
if (!require('plotly', quietly = T)) install.packages('plotly');
if (!require('htmlwidgets', quietly = T)) install.packages('htmlwidgets');
if (!require('ggplot2', quietly = T)) install.packages("ggplot2");

library(tibble)
library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(stringr)
library(stringi)
library(DT)
library(plotly)
library(htmlwidgets)
library(ggplot2)

#####################################################################
# Usefull functions
#####################################################################
download <- function(title,plotname){
        downloadHandler(
                filename = function() { title },
                content = function(file) {
                        ggsave(file, plot = plotname, device = "png")
                        }
                )
}

############################################################################################################################

# Debut
function(input, output) {
  
  
  observeEvent(input$sidebarID, {
    js$scrolltop()
  })
}

